FORMAT: 1A

# Flightsayer Flight Subscriptions API

Flightsayer's flights API allows consumers to subscribe to push notifications so that they are automatically notified when flight status (including delay prediction) changes.

The api lives at `https://api.flightsayer.com`. 

Check out the pull version of the flight status API [here](flight_status.md)
 
# Subscriptions [/subscriptions]

The subscriptions endpoint lists all flight status update subscriptions, and allows you to create, update, and delete subscriptions. When you subscribe to a flight status update, you will receive push notifications whenever the status of the flight in question changes, sent to a specified URL. Note that the flight subscription will be automatically deleted after the final POST request is sent to the target URL (this occcurs after the flight lands or is cancelled).

## Retrieve all subscriptions [GET /subscriptions/]
Retrieve all current flight subscriptions

+ Request (application/json)

    + Headers

            Token: thisisasampletoken

+ Response 200 (application/json)

        + Attributes
            + count (number, required) - number of flights matching the filter
            + next (string, optional) - url pointing to the next set of paginated results
            + previous (string, optional) - url pointing to the previous set of paginated results
            + results (array[FlightSubscription]) - an array of FlightSubscription objects

        + Body
                {
                    "count": 1,
                    "next": null,
                    "previous": null,
                    "results": [
                    {
                      "flight_id": "UA1261ORDEWR1607080215",
                      "target": "http://status.concernedpassenger.com",
                      "created": "2016-07-11T21:54:22Z",
                      "updated": "2016-07-11T21:54:22Z"
                    }
                  ]
                }

## Subscription for a specific flight [/subscriptions/{flight_id}]

    + Parameters
        + flight_id: UA576BOSSFO1606092145 (FlightId, required)

    + Headers

            Token: thisisasampletoken
				
### Create or update a subscription for the specified flight [PUT /subscriptions/{flight_id}]

+ Request (application/json)

     + Attributes
        + target (string, required) - url to which POST requests indicating change to flight status will be sent.

    + Body

            {
                "target": "http://status.concernedpassenger.com"
            }

+ Response 201 (application/json)
New subscription created.

    + Attributes (FlightSubscription)

    + Body

            {
                "flight_id": "WN2379SJCSAN1609282300",
                "target": "http://test3.com",
                "created": "2016-07-12T19:26:23Z",
                "updated": "2016-07-12T19:29:34Z"
            }

+ Response 200 (application/json)
Existing subscription updated

    + Attributes (FlightSubscription)

### Retrieve a subscription [GET /subscriptions/{flight_id}]
Retrieve a subscription for the specified flight.

+ Response 200 (application/json)

    + Attributes (FlightSubscription)
    
    + Body
    
            {
                "flight_id": "WN2379SJCSAN1609282300",
                "target": "http://test3.com",
                "created": "2016-07-12T19:26:23Z",
                "updated": "2016-07-12T19:29:34Z"
            }


### Delete a subscription [DELETE /subscriptions/{flight_id}]
Delete the subscription for the specified flight.

+ Response 204 (application/json)

# Data Structures

## FlightId (string)
A flight id uniquely represents a flight, and takes the form: [IATA carrier code][flight number][departure airport][arrival airport][scheduled departure time as YYMMDDHHMM in UTC time].
For example: `UA576BOSSFO1606092145`

## FlightSubscription (object)
A subscription for flight status alerts
+ Attributes
    + flight_id (FlightId, required) - flight ID such as `UA1261ORDEWR1607080215`
    + target (string, required) - target URL where updates to flight status are sent as a POST request
    + created (timestamp, required) - timestamp at which the subscription was created
    + updated (timestamp, required) - timestamp at which the subscription was last updated
    
