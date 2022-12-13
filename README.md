Unison Local UI
===============

[![CI](https://github.com/unisonweb/unison-local-ui/actions/workflows/ci.yml/badge.svg)](https://github.com/unisonweb/unison-local-ui/actions/workflows/ci.yml)

This is the UI you see when you type `ui` into `ucm`.

Running Development Server
--------------------------

🔔 You should only need to run the UI development server if you're contributing to the UI. If you just want to run the UI to see your Unison codebase, it should either come pre-installed with `ucm` or if you built from source can be downloaded with a script: `./dev-ui-install.hs` from the `unison` repository ([Running Unison](https://github.com/unisonweb/unison/blob/trunk/development.markdown#running-unison)).

1. Start `ucm` (the executable is `unison` instead of `ucm` if you built the [unison repository](https://github.com/unisonweb/unison) from source) in headless mode: `ucm headless`, and copy the API URL (this URL
   is uniquely generated by `ucm` at start-up) from the `ucm` start-up output
   (`ucm` also outputs a UI URL, but this isn't used when developing for the
   UI/running the UI from this repository).

2. Make sure the latest dependencies are installed with by running `npm install` followed by `npm run ui-core-install`.

3. Start the dev server with: `API_URL="<API URL FROM UCM>" npm start`

4. Visit `http://localhost:1234` in a browser.

Dependencies
------------

This depends on the [ui-core package](https://github.com/unisonweb/ui-core) via
[elm-git-install](https://github.com/robinheghan/elm-git-install). That package
includes both the Unison design system, and a core set of components for
working with and rendering Unison definitions and
namespaces.

Bumping [ui-core package](https://github.com/unisonweb/ui-core)
---------------------------------------------------------------

The UI Core dependency can be updated to its latest version with this command:

```bash
npm run ui-core-update
```

To install a specific sha:

```bash
npm run ui-core-install -- [SOME_UI_CORE_SHA]
```

Community
--------
[Code of conduct](https://www.unisonweb.org/code-of-conduct/)
