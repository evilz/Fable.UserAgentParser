# Fable.UserAgentParser


# Dev notes 

install npm packages
`npm init`
`npm install  @babel/core ava fable-compiler fable-compiler-js fable-publish-utils fable-splitter`

add script commands

```
"scripts": {
    "build": "fable build.fsx --run",
    "publish": "fable build.fsx --run publish",
    "test": "dotnet build src && fable-splitter test/Test.fsproj --commonjs && ava test/bin/Test.js"
  },
```

create build.fsx use for publish


create directories  `src` and `test`

