---
date: 2024-01-20 17:43:37.240018-07:00
description: 'How to: In Clojure, you can use `clj-http` to quickly download a web
  page. Here''s a barebones example.'
lastmod: '2024-03-13T22:44:59.744028-06:00'
model: gpt-4-1106-preview
summary: In Clojure, you can use `clj-http` to quickly download a web page.
title: Downloading a web page
weight: 42
---

## How to:
In Clojure, you can use `clj-http` to quickly download a web page. Here's a barebones example:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Use it like this:
(defn -main []
  (println (download-page "http://example.com")))
```

If you try that out, you'll get a map full of details. The juicy bits are under `:body` and `:status`.

## Deep Dive
Historically, web downloading was a 'wget' or 'curl' at the command line. Now, languages like Clojure abstract this with libraries. `clj-http` is one such library that wraps Java's Apache HttpComponents for Clojure's functional style.

Alternatives? Sure. You could conjure up `java.net.HttpURLConnection` directly or pick another library like `http-kit` – but `clj-http` is comfy and packs most things you'll need out of the box.

As for nuts and bolts, `clj-http` turns your request into a Java HTTP entity, makes the call, and hands the response back. Behind the scenes, it's handling redirects, parsing headers, and managing the response body so you can focus on your data, not the plumbing.

## See Also
- clj-http GitHub repo: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure http-kit for another approach: [http://www.http-kit.org](http://www.http-kit.org)
- Official Clojure site for more on the language: [https://clojure.org](https://clojure.org)
