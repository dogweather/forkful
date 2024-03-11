---
date: 2024-02-03 19:02:47.953382-07:00
description: "Regular expressions, a powerful tool for pattern matching and data manipulation,\
  \ are essential in text processing tasks such as validating input,\u2026"
lastmod: '2024-03-11T00:14:33.587447-06:00'
model: gpt-4-0125-preview
summary: "Regular expressions, a powerful tool for pattern matching and data manipulation,\
  \ are essential in text processing tasks such as validating input,\u2026"
title: Using regular expressions
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, a powerful tool for pattern matching and data manipulation, are essential in text processing tasks such as validating input, searching, and replacing text. Programmers use them extensively for handling complex string parsing and data validation tasks efficiently and succinctly.

## How to:
Clojure, staying true to its roots in the Lisp family, offers a rich set of functions that interface seamlessly with Java's regular expression capabilities. Here's how you can leverage them:

### Basic Matching
To check if a string matches a pattern, use `re-matches`. It returns the entire match if successful or `nil` otherwise.

```clojure
(re-matches #"\d+" "123")  ;=> "123"
(re-matches #"\d+" "abc")  ;=> nil
```

### Searching for Patterns
To find the first occurrence of a pattern, `re-find` is your go-to function:

```clojure
(re-find #"\d+" "Order 123")  ;=> "123"
```

### Capturing Groups
Use `re-find` along with parentheses in your pattern to capture groups:

```clojure
(let [[_ area code] (re-find #"(1)?(\d{3})" "Phone: 123-4567")]
  (println "Area Code:" area "Code:" code))
;; Output: Area Code: nil Code: 123
```

### Global Search (Find All Matches)
Clojure doesn’t have a built-in global search like some languages. Instead, use `re-seq` to get a lazy sequence of all matches:

```clojure
(re-seq #"\d+" "id: 123, qty: 456")  ;=> ("123" "456")
```

### Splitting Strings
To split a string based on a pattern, use `clojure.string/split`:

```clojure
(clojure.string/split "John,Doe,30" #",")  ;=> ["John" "Doe" "30"]
```

### Replacing
Replace parts of a string matching a pattern with `clojure.string/replace`:

```clojure
(clojure.string/replace "2023-04-01" #"\d{4}" "YYYY")  ;=> "YYYY-04-01"
```

### Third-party Libraries
Though Clojure’s built-in support suffices for most cases, for more complex scenarios, consider using libraries such as `clojure.spec` for robust data validation and `reagent` for reactive DOM manipulation in web applications with regex-based routing and input validation.

```clojure
;; Example using clojure.spec for validating an email
(require '[clojure.spec.alpha :as s])
(s/def ::email (s/and string? #(re-matches #".+@.+\..+" %)))
(s/valid? ::email "test@example.com")  ;=> true
```

Remember, while regular expressions are powerful, they can also make code hard to read and maintain. Use them judiciously and always consider simpler string manipulation functions where possible.
