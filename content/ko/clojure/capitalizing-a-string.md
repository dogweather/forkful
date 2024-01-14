---
title:    "Clojure: 문자열 대문자로 바꾸기"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## 왜

스트링을 캐피타라이징하는 방식으로 코딩을 해보길 추천해요. 이를 통해, 당신은 다양한 프로그래밍 언어에서 문자열을 더 쉽게 다룰 수 있게 될 거예요.

## 어떻게

```Clojure
;; 스트링을 캐피타라이즈하는 함수 만들기
(defn capitalize-string [str]
  (str/upper-case str))

;; 캐피타라이즈 함수 실행
(capitalize-string "hello world") ; "HELLO WORLD"

;; 여러 단어를 포함하는 경우에도 적용 가능
(capitalize-string "i love clojure") ; "I LOVE CLOJURE"
```

## 깊게 파헤치기

캐피타라이즈 함수는 문자열을 대문자로 바꾸어주는 간단한 기능을 가지고 있어보이지만, 실제로는 여러가지 다양한 사용 방법이 있을 수 있어요. 예를 들어, 캐릭터 셋이나 locale 설정을 고려해야하는 경우에도 적용 가능하죠. 또한, 정규 표현식과 조합하여 원하는 패턴의 문자열만 캐피타라이즈 할 수도 있어요. 이러한 다양한 활용 방법을 익혀두면 코딩할 때 더욱 효율적인 방식으로 문자열을 다룰 수 있게 될 거예요.

## 더 알아보기

[공식 Clojure 문서 - 문자열 함수 사용법](https://clojure.org/reference/strings)  
[Clojure Cookbook - 문자열 다루기](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/06_strings.asciidoc)  

## 관련글

[스택 오버플로우 - Clojure에서 문자열 캐피타라이즈하는 방법](https://stackoverflow.com/questions/3355123/how-to-capitalize-sequence-of-words-in-a-string-using-clojure)