---
title:                "Clojure: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

이 글에서는 특정 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보겠습니다. 이 기능을 사용하면 데이터를 정리하고 원하지 않는 문자를 제거할 수 있습니다.

## 사용 방법

우선, 우리는 `string` 변수에 문자열을 할당할 것입니다.

```Clojure
(def string "Hello World! 안녕하세요!")
```

그리고 `remove` 함수를 사용해 특정 패턴과 일치하는 문자를 제거합니다.

```Clojure
(def result (remove #{\!} string))
```

코드를 실행하면 다음과 같은 결과가 출력됩니다.

```Clojure
;=> "Hello World 안녕하세요"
```

여기서 `#{\!}`는 삭제하고 싶은 문자를 나타냅니다. 만약 더 많은 문자를 삭제하고 싶다면 다음과 같이 작성할 수 있습니다.

```Clojure
(remove #{\! \?} string)
```

그리고 `filter` 함수를 사용하면 특정 패턴과 일치하는 문자를 제거하는 것이 아닌, 해당 패턴과 일치하는 문자만 남길 수 있습니다.

```Clojure
(filter #{\! \?} string)
```

결과는 다음과 같습니다.

```Clojure
;=> "!?"
```

## 딥 다이브

Clojure에서 `remove` 함수는 주어진 시퀀스에서 특정 항목을 제외하고 새로운 시퀀스를 반환합니다. 이 함수는 `filter` 함수와 유사하지만, 항목을 걸러내는 대신 선택적으로 포함시킵니다.

## 참고 자료

- [Official Clojure Documentation on `remove`](https://clojure.org/api/cheatsheet#_remove)
- [Blog post on Clojure string manipulation](https://www.martinklepsch.org/posts/clojure-string-manipulation.html)
- [StackOverflow thread on removing characters from string](https://stackoverflow.com/questions/4212662/how-to-remove-characters-from-a-string-in-clojure)