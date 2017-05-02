FORMAT: 1A

# Flightsayer Flight Subscriptions API

Flightsayer's flights API allows consumers to subscribe to push notifications so that they are automatically notified when flight status (including delay prediction) changes.

The api lives at `https://api.flightsayer.com/flights/v1`. 

### Quick start 
To track flight United 1122 on 5/29/17 (flight id `UA1122DENBOS1705291425`) and receive push notifications at `http://target-url.com` (this is your server):
```
curl -X POST -H 'Token: <api-token>' -H 'Content-Type: application/json' \
    -d '{"flight_ids": ["UA1122DENBOS1705291425"], target": "http://target-url.com"}' \
    https://api.flightsayer.com/flights/v1/subscriptions/
```
where `api-token` is the API token you have been provided (contact `info@flightsayer.com` if you need an API token).

### For info on Check out the pull version of the flight status API [here](flight_status.md)
 
# Subscriptions [/subscriptions]

The subscriptions endpoint allows customers to subscribe for push notifications for flights, and to manage their existing notifications.

### Retrieve all subscriptions [GET /subscriptions/]
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
                      "itinerary_id": "9bb348b3-d567-4392-818f-e19b7ad7fbf2",
                      "flight_ids": ["AS22SEAORD1705021910", "B6112ORDBOS1705030035"],
                      "target": "http://requestb.in/wr9k2hwr",
                      "created": "2017-05-02T16:26:02.883037",
                      "updated": "2017-05-02T16:26:02.883060"
                    }
                  ]
                }

### Create a subscription for the specified itinerary [POST /subscriptions/]
To subscribe to push notifications for an itinerary, first obtain valid flight IDs for the flight and a target URL

+ Request (application/json)

    + Headers
    
            Token: thisisasampletoken
            
    + Attributes
     
        + flight_ids (array[FlightId], required) - an array of flight IDs that represent a single itinerary
        + target (string, required) - url to which POST requests indicating change to flight status will be sent.


    + Body

            {
                "flight_ids": ["B6112ORDBOS1705030035", "AS22SEAORD1705021910"]
                "target": "http://status.concernedpassenger.com"
            }

+ Response 201 (application/json)
New subscription created.

    + Attributes (FlightSubscription)

    + Body

            {
                "itinerary_id": "9bb348b3-d567-4392-818f-e19b7ad7fbf2",
                "flight_ids": ["AS22SEAORD1705021910", "B6112ORDBOS1705030035"],
                "target": "http://requestb.in/wr9k2hwr",
                "created": "2017-05-02T16:26:02.883037",
                "updated": "2017-05-02T16:26:02.883060"
            }

### Retrieve a subscription [GET /subscriptions/{itinerary_id}]
Retrieve a subscription for a specific itinerary

+ Request (application/json)

    + Parameters
        + itinerary_id: 9bb348b3-d567-4392-818f-e19b7ad7fbf2 (string, required) - uuid representing an itinerary

    + Headers
    
            Token: thisisasampletoken

+ Response 200 (application/json)

    + Attributes (FlightSubscription)
    
    + Body
    
                {
                    "itinerary_id": "cabb3527-3c77-488e-8306-f8bf38a904ef",
                    "flight_ids": ["UA619SEAORD1705030620"],
                    "target": "http://requestb.in/wr9k2hwr",
                    "created": "2017-05-02T16:42:17.650792",
                    "updated": "2017-05-02T16:42:17.650812"
                }


### Delete a subscription [DELETE /subscriptions/{itinerary_id}]
Delete the subscription for the specified itinerary.

+ Request (application/json)

    + Parameters
        + itinerary_id: 9bb348b3-d567-4392-818f-e19b7ad7fbf2 (string, required) - uuid representing an itinerary

    + Headers
    
            Token: thisisasampletoken
            
+ Response 204 (application/json)

# Data Structures

## FlightId (string)
A flight id uniquely represents a flight, and takes the form: [IATA carrier code][flight number][departure airport][arrival airport][scheduled departure time as YYMMDDHHMM in UTC time].
For example: `UA576BOSSFO1606092145`

## FlightSubscription (object)
A subscription for flight status alerts
+ Attributes
    + itinerary_id (string, required) - uuid such as `cabb3527-3c77-488e-8306-f8bf38a904ef` identifying the itinerary
    + flight_ids (array[FlightId], required) - array of flight IDs representing the flights in this itinerary
    + target (string, required) - target URL where updates to flight status are sent as a POST request
    + created (timestamp, required) - timestamp at which the subscription was created
    + updated (timestamp, required) - timestamp at which the subscription was last updated
