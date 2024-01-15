---
title:                "랜덤 숫자 생성하기"
html_title:           "Clojure: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜

왜이 아픠 때문에 난수를 생성하는 것에 놀라게 될 수 있습니다. 그냥 새로운 코딩 기술을 배우는 것보다 더 나은 이유는 이기쁜 일이 없어요!

## 하는 법

Here's an easy way to generate random numbers in Clojure:

 ``` Clojure
 (rand) 
 ;; 0.6875913269905133

(random 1 10) 
;; 4 
 ```

These functions can take in arguments to specify the range and type of random number to be generated. For example, `rand` generates a random floating-point number between 0 (inclusive) and 1 (exclusive), while `random` takes in the start and end numbers of the range and returns a random integer within that range.

## 깊이 들어가기

Clojure에서 난수를 생성하는 더욱 자세한 정보를 알고 싶다면 [Clojure 공식 문서](https://clojuredocs.org)에서 `rand`와 `random` 함수를 찾아보세요. 이 함수들은 어떤 서수열을 기반으로 난수를 생성하는지와 그 로직에 대해서도 자세히 설명하고 있습니다. 또한 Clojure의 랜덤 함수는 입력값에 대한 캐싱(cache) 및 동시성 이슈에 대한 자세한 내용도 참고할 수 있습니다. 

## 참고 자료

* [Clojure 공식 문서](https://clojuredocs.org)
* [Clojure 코드의 난수 생성](https://clojure.org/guides/random_numbers)