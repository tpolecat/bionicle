---
layout: home

---
# bionicle

<img align="right" src="https://cdn.rawgit.com/tpolecat/bionicle/series/0.5.x/bionicle_logo.svg" height="150px" style="padding-left: 20px"/>

[![Travis CI](https://travis-ci.org/tpolecat/bionicle.svg?branch=series%2F0.5.x)](https://travis-ci.org/tpolecat/bionicle)
[![Join the chat at https://gitter.im/tpolecat/bionicle](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/tpolecat/bionicle?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Maven Central](https://img.shields.io/maven-central/v/org.tpolecat/bionicle-core_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/org.tpolecat/bionicle-core_2.12)
[![Javadocs](https://javadoc.io/badge/org.tpolecat/bionicle-core_2.12.svg)](https://javadoc.io/doc/org.tpolecat/bionicle-core_2.12)


How about some text here? And a snippet.

```tut
"hello bionicle"
```

## Quick Start

The current development version is **{{site.bionicleVersion}}** for **Scala {{site.scalaVersions}}** with

- [**cats**](http://typelevel.org/cats/) {{site.catsVersion}}

To use **bionicle** you need to add the following to your `build.sbt`.

```scala
libraryDependencies ++= Seq(
  "org.tpolecat" %% "bionicle-core" % "{{site.bionicleVersion}}"
)
```

See the [**documentation**](docs/01-foo.html) for more information.

Note that **bionicle** is pre-1.0 software and is still undergoing active development. New versions are **not** binary compatible with prior versions.

## Documentation and Support

- Behold the sparkly [**documentation**](docs/01-foo.html) ← start here
- The [**Scaladoc**](https://www.javadoc.io/doc/org.tpolecat/bionicle-core_2.12) will be handy once you get your feet wet.
- See the [**changelog**](https://github.com/tpolecat/bionicle/blob/series/0.5.x/CHANGELOG.md) for an overview of changes in this and previous versions.
- The [**Gitter Channel**](https://gitter.im/tpolecat/bionicle) is a great place to chat!
- There is also the [**source**](https://github.com/tpolecat/bionicle).
- If you have comments or run into trouble, please file an issue.
