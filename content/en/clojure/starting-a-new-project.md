---
title:    "Clojure recipe: Starting a new project"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why
Programming in Clojure is an exciting and rewarding experience for developers looking to create powerful and efficient applications. With its functional programming approach, robust standard library, and powerful concurrency features, using Clojure can give your project a cutting-edge advantage. In this blog post, we will explore the process of starting a new project in Clojure, so you can jump right into creating amazing software.

## How To
To start a new project in Clojure, you will need to have the Clojure command-line tools installed on your machine. Once you have that set up, you can use the ```clj``` command to create a new project directory, with the following command:

```Clojure
clj -X:new :template app :name my-project
```

This will create a new project called "my-project" with all the necessary files and folders. Next, navigate into the project directory and open the ```deps.edn``` file. This is where you can specify any dependencies or plugins you want to use in your project. For example, if you want to use the popular library "Ring" for web development, you can add it to your dependencies like this:

```Clojure
:deps [[ring/ring "1.9.0"]]
```

Save the file, and now your project is ready to be built and run. To do so, use the following command:

```Clojure
clj -M -m projectName.core
```

This will run your project's core namespace, which is the starting point of your application. Now you can start coding your project using the powerful features of Clojure.

## Deep Dive
When starting a new project in Clojure, it's essential to understand the structure of the project directory. Inside the project, you will see a folder called ```src```. This is where all your application code will reside. You will also see a file named ```core.clj```, which is the starting point of your project. This file contains a namespace declaration that looks like this:

```Clojure
(ns projectName.core)
```

This indicates that this file belongs to the "projectName" project and is named "core." You can create as many namespaces as you need to organize your code. For example, if you want to create a "util" namespace to hold all your utility functions, you can add it to your project like this:

```Clojure
(ns projectName.util)
```

You can then require this namespace in your ```core.clj``` file like this:

```Clojure
(require '[projectName.util :refer [my-util-fn]])
```

This will allow you to use the functions from the "util" namespace in your project's core namespace.

## See Also
- [Official Clojure website](https://clojure.org/) 
- [Clojure docs](https://clojure.org/documentation) 
- [Clojure for the Brave and True](https://www.braveclojure.com/)