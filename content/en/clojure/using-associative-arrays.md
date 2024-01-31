---
title:                "Using associative arrays"
date:                  2024-01-30T18:57:38.017965-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, or hash maps, in Clojure allow you to store and retrieve data with key-value pairs. They are a go-to for managing structured data, making it quicker to access specific elements without iterating through a list.

## How to:

In Clojure, creating and manipulating associative arrays (hash maps) is straightforward. Let's dive in with examples.

To create a hash map:

```clojure
(def my-map {:name "Alex" :age 30})
```

You can retrieve a value by specifying its key:

```clojure
(get my-map :name)
;; "Alex"
```
Or, more idiomatically, you can use the key as a function:

```clojure
(:name my-map)
;; "Alex"
```

Adding or updating entries is simple:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

For removing keys, use `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

To iterate over a map:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

And for conditional access, `find` returns a key-value pair if the key exists:

```clojure
(find my-map :age)
;; [:age 30]
```

## Deep Dive

Associative arrays in Clojure, also commonly referred to as hash maps, are incredibly versatile and efficient for managing key-value based data. They are part of Clojure's rich collection library, deeply rooted in the language's philosophy of immutability and functional programming. Unlike arrays or lists that require O(n) time complexity for accessing elements, hash maps provide near-constant time complexity for access, making them highly efficient for look-up operations.

One might argue that vectors in Clojure could serve a similar purpose through indexed access, but hash maps shine when it comes to dealing with non-sequential and labeled data, where the key provides a meaningful descriptor rather than an arbitrary index.

Unique to Clojure (and its Lisp heritage), associative arrays are first-class citizens, meaning they can be directly manipulated, passed around functions, and more, without needing special syntax or access methods. This design decision reinforces Clojure's emphasis on simplicity and power.

While hash maps are incredibly useful, it's worth mentioning that for very large datasets or scenarios where keys are highly dynamic (constant addition and removal), alternative data structures or databases might offer better performance and flexibility. However, for most typical use cases within the realm of Clojure applications, associative arrays provide a robust and efficient means of data management.
