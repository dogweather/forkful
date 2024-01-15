---
title:                "디렉토리 존재 여부 확인하기"
html_title:           "Clojure: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
mkdir이 디렉터리를 만들고, 그 디렉터리가 이미 존재하는지 확인하는 것은 잘못된 경로를 예방하고 반복되는 디렉터리를 피하기 위해 필수적입니다.

## 하는 방법
```Clojure
(ns example.core
  (:require [clojure.java.io :as io]))

(defn check-dir-exists [path]
  (if (.exists (io/file path))
    (println "Directory exists!")
    (println "Directory does not exist.")))
```

`(check-dir-exists "example/directory")` 을 실행하면 `Directory exists!` 가 출력되는 것을 볼 수 있습니다.

## 깊게 파고들기
`clojure.java.io` 네임스페이스에는 파일 및 디렉터리를 다루는 함수들이 있습니다. 이 네임스페이스를 사용하기 위해서는 `require`를 사용하여 불러와야 합니다. `(.exists (io/file path))` 는 지정된 경로의 파일이나 디렉터리가 실제로 존재하는지를 확인하는데 사용됩니다.

See Also
[Official documentation for clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
[Clojure Cookbook: Working with Files and Directories](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/working-with-files-and-directories/working-with-files-and-directories.adoc)