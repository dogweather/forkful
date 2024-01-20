---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is simply verifying the presence of a specific folder in a given path by a program. It’s a common task that helps avoid filesystem-related errors, making a program more reliable and robust.

## How to:

In Clojure, the java.nio.file namespace can be used to check if a directory exists. You can use the `Files/exists` method along with `Paths/get` to check. Here's how to do it:

```Clojure
(import '[java.nio.file Paths Files])

(defn dir-exists? [dir]
  (Files/exists (Paths/get dir (into-array String []))))
```

The function `dir-exists?` will return `true` if the directory exists and `false` otherwise. For instance:

```Clojure
(dir-exists? "/home/user/Documents") 
;=> true or false depending on whether /home/user/Documents exists
```

## Deep Dive

Checking if a directory exists is not an invention of Clojure or any modern programming language. It's deeply rooted in the old-school file handling operations from the days of C and UNIX filesystems.

The straightforward `Files/exists` method used above, however, is not the only way you can handle this. For example, you could use java.io.File's `.exists` method:

```Clojure
(.exists (java.io.File. "path to directory"))
```

Be aware of I/O operations performance implications. While these operations are generally quick, overuse or incorrectly using them could hurt your application's speed.

Furthermore, note `Files/exists` might not work as expected with symbolic links due to specific filesystems' nuances. Consider using `Files/isDirectory` with the `LinkOption/NofollowLinks` to avoid resolving symbolic links.

## See Also

To further your knowledge in Clojure's filesystem interactions:

1. [Clojure.java.io documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
3. [java.nio.file API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/package-summary.html)