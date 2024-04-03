---
date: 2024-01-26 00:52:05.272648-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694: \uD074\uB85C\uC800\uB294 \uB9AC\
  \uC2A4\uD504 \uACC4\uC5F4 \uC5B8\uC5B4\uB4E4\uCC98\uB7FC \uC608\uC678\uB97C \uC0AC\
  \uC6A9\uD558\uC5EC \uC5D0\uB7EC\uB97C \uCC98\uB9AC\uD569\uB2C8\uB2E4. \uC5EC\uAE30\
  \ \uAC00\uB294 \uAE38\uC5D0\uC11C \uBB38\uC81C\uAC00 \uC0DD\uACBC\uC744 \uB54C \uC5B4\
  \uB5BB\uAC8C \uB300\uCC98\uD558\uB294\uC9C0 \uBCF4\uC5EC\uC8FC\uB294 \uBC29\uBC95\
  \uC785\uB2C8\uB2E4. \uC608\uC678\uB97C \uBC1C\uC0DD\uC2DC\uD0A4\uB294 \uAC83\uC740\
  \ \uC9C1\uAD00\uC801\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.671745-06:00'
model: gpt-4-1106-preview
summary: "\uD074\uB85C\uC800\uB294 \uB9AC\uC2A4\uD504 \uACC4\uC5F4 \uC5B8\uC5B4\uB4E4\
  \uCC98\uB7FC \uC608\uC678\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC5D0\uB7EC\uB97C \uCC98\
  \uB9AC\uD569\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 어떻게 하나요:
클로저는 리스프 계열 언어들처럼 예외를 사용하여 에러를 처리합니다. 여기 가는 길에서 문제가 생겼을 때 어떻게 대처하는지 보여주는 방법입니다.

예외를 발생시키는 것은 직관적입니다:
```Clojure
(throw (Exception. "이런! 무언가 잘못되었습니다."))
```

예외를 처리하는 것, 자주 하게 될 것입니다:
```Clojure
(try
  ;; 위험한 코드
  (/ 1 0)
  (catch ArithmeticException e
    (println "0으로 나눌 수 없습니다!"))
  ;; finally 블록은 어떤 상황이든 실행됩니다
  (finally 
    (println "정리 코드가 여기에 들어갑니다.")))
```
위 catch 블록의 샘플 출력:
```
0으로 나눌 수 없습니다!
정리 코드가 여기에 들어갑니다.
```

예외에 대한 더 풍부한 문맥을 제공하기 위해 `ex-info`와 `ex-data`를 사용하기:
```Clojure
(try
  ;; 사용자 정의 예외를 발생시키기
  (throw (ex-info "사용자 정의 오류" {:type :custom-failure}))
  (catch Exception e
    ;; 사용자 정의 예외에서 데이터를 가져오기
    (println (ex-data e))))
```
샘플 출력:
```
{:type :custom-failure}
```

## 심층 탐구
클로저의 에러 처리는 다른 리스프들이나 심지어 자바(여기서 `try-catch` 메커니즘을 물려받음)와 비교해 볼 때 혁신적으로 다르지 않습니다. 실용적입니다; 예외 사용은 주된 길입니다, 바로 자바처럼요, 하지만 클로저는 더 풍부한 에러 데이터를 제공하기 위해 `ex-info`와 `ex-data`를 포함한 함수형 맛을 제공합니다.

클로저에서 에러 처리를 위한 다른 방법들로는 `cats`와 같은 라이브러리에서 제공되는 `either` 모나드와 같은 모나딕 구조를 사용하는 방법, 혹은 채널 기반 에러 전파를 위한 core.async를 이용하는 방법 등이 있습니다. 하지만 이들은 더 복잡하고 특정 시나리오에서 사용됩니다.

역사적으로 프로그래밍 언어에서 에러 처리는 단순한 상태 반환에서 현대 언어들의 더 정교한 예외 처리 메커니즘으로 진화했습니다. 클로저는 단순성을 추구하며 함수형 프로그래밍을 조금 섞어 고전과 현대를 혼합합니다.

## 또한 보기
- 클로저의 예외 안내서: https://clojure.org/guides/exceptions
- 더 함수형적인 접근을 위한 "Cats" 라이브러리: https://github.com/funcool/cats
- 비동기 프로그래밍을 위한 "Core.async": https://github.com/clojure/core.async
