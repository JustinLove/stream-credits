# StreamCredits

Scrolling stream credits. Shows channels hosting at time of load, and follows that occurred since start of currently running stream (by Twitch API)

## Url Paramters

Once channel name is entered, it will update the url, this can be bookmarked or used as a browser scene in OBS etc.

- `login`: channel name to report on
- `userId`: channel user id for api calls, login will be converted to user id if needed

## Raids: Chat Events

The application will update data on visible change (e.g. streaming software scene change) It will also monitor chat for raids while running, so for this feature it should not be set to unload when not visible.

## Compiling

Built using [Elm](http://elm-lang.org/)

My build command:

> `elm make src/StreamCredits.elm --output public/stream-credits.js`

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
