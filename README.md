Dimple2
=======

An implementation Dimple-II membership protocol for epidemic protocols
with support for automatic discovery. Dimple2 is an library designed
to be used from within a haskell program. However, the Dimple2 peers
interact with one another through a REST api and that REST api has
been extended to support all Dimple2 interactions.

The membership protocol is full discussed in the dimple-ii.pdf
document in the doc directory and can also be readily found with an
internet search for 'Dimple-II membership protocol'. The exception to
this is the discovery protocol which will be described here.

Automatic Discovery
-------------------

When the Dimple2 system is started it sends out a multicast broadcast
containing the URL in
[Netstrings](http://cr.yp.to/proto/netstrings.txt) format. Peers that
hear that broadcast should optionally `POST` their URL to the
`interested` peer API. Regardless of the number of nodes that hear
this broadcast no more then about 10 - 100 nodes should respond. This
implementation uses a probabilist algorythim based on the estimated
number of nodes in the system to ensure that property.

### Algorithym

0. Check to see if there are any peers in the `localview`
1. If peers exist wait N seconds and try again
2. If no peers exist, Broadcast Peer URL in Netstrings format
4. Collect responses from interested parties for N seconds
5. If interested parties have responded, start join operation
6. If interested parties have not responded start broadcast process again

Manual Discovery
----------------

To manually force discovery you may put a url end point to the local
view. This will have a high probability of forcing a shuffle to occur.

REST API
--------

*All json descriptions are in the format of js-schema*. They should be
readable on their own, but more information can be found at the
[js-schema](https://github.com/molnarg/js-schema/) site.

### Types used throughout

    var URL = String;

    var Peer = schema ({url : URL,             // The url of the peer
                        age : Number.min(0)}); // the number of failed
                                               // sync attemps

     var Interested = URL;                     // url of the node interested
                                               // in communicating

### Interested Nodes Collection

    /dimple2/interested
      GET - Get the list of interested nodes
      POST - Post a new nod to the list of interested nodes

#### `POST`


This api exists to allow for automated discovery. Peers interested in
participating are expected to `POST` a value represented by the
`Interested` scheme to `/dimple2/interested`.

#### `GET`

     Array.of(Interested)

This api returns a list of urls that are currently in the `interested`
list of this target Peer. *NOTE* The vast majority of the time the
`interested` list is empty! It is only populated in the short period
of time between when a Peer begins the discovery process and starts
interacting with the discovered peers. That being the case this
operation is probably only useful during development.

### Local View Collection

    /dimple2/localview
      GET - get the list of Peers in the local view

This allows a caller to get the current list of urls in this peers
local view. This is not used as part of the shuffle algorithym, but
simple exists for informational and monitoring purposes. This returns
the following

    Array.of(Peer)

This may take 1 parameter `oldest=true`. This will return an array of
a single peer that is the current oldest member of the local view.

### Local View Item

    /dimple2/localview/{url}
      PUT - Put the url indentified by {url}, initiating a shuffle response

This starts a shuffle process. The body of the `PUT` request should be
a `Peer` object. See the Dimple-II, Algorithym 4,5, and 6 for details.

### Join Local View Collection

    /dimple2/joinlocalview
      GET - get the generated local view

Provides a generated local view as described in Dimple-II, Algorythim 7.

### Node Estimate

    /dimple2/nodeestimate
      GET - provides the current estimate of the number of nodes in
            the system. See Dimple-II Algorythim 8.
