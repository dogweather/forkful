---
title:                "문자열 추출"
html_title:           "Clojure: 문자열 추출"
simple_title:         "문자열 추출"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가가 substring을 추출하는 것에 관심을 가질까요? substring 추출은 문자열을 조작하는 엄청난 힘을 부여해줍니다. 예를 들어, substring을 사용하면 문자열을 분석하고 원하는 정보를 가져오는 등 다양한 작업을 할 수 있습니다.

## 하는 방법

사용자가 입력한 문자열에서 특정 부분만 추출하는 방법을 알아보겠습니다. 다음은 Clojure에서 substring을 추출하는 코드 예시입니다.

```Clojure
;; "Clojure Programming"이라는 문자열에서 "Programming" 부분만 추출하기

(def string "Clojure Programming")

(subs string 8) ; 8번째 인덱스부터 끝까지 반환
;; => "Programming"

(subs string 8 17) ; 8번째 인덱스부터 17번째 인덱스 이전까지 반환
;; => "Program"
```

위 코드에서 subs 함수로 문자열과 추출할 인덱스를 전달하여 substring을 추출할 수 있습니다. 만약 추출할 인덱스를 지정하지 않으면 문자열의 첫번째 인덱스부터 끝까지 추출합니다. 또한 두 번째 인자로 전달한 인덱스 이전까지만 추출할 수도 있습니다.

## 깊이 파고들기

substring을 추출하는 방법은 다양한 옵션을 가지고 있습니다. 예를 들어, 인덱스 대신 문자열의 길이를 전달하여 추출할 수도 있습니다. 또한 정규식 패턴을 이용하여 원하는 문자열만 추출할 수도 있습니다.

또한 substring 추출은 데이터 분석에 유용하게 사용될 수 있습니다. 예를 들어, CSV 파일에서 특정 열의 데이터만 추출해낼 때 substring을 사용할 수 있습니다. Clojure의 다양한 문자열 함수를 활용하여 더욱 복잡한 작업도 가능합니다.

## 관련 자료

- [Clojure Strings Documentation](https://clojuredocs.org/clojure.string)
- [Clojure Applied: From Practice to Practitioner](https://www.amazon.com/Clojure-Applied-Practice-Practitioner-Programmer/dp/1680500740)
- [How to Use Substring in Clojure?](https://stackoverflow.com/questions/45072810/how-to-use-substring-in-clojure)