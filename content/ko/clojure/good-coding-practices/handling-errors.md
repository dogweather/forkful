---
date: 2024-01-26 00:52:05.272648-07:00
description: "\uC5D0\uB7EC \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C\
  \ \uC608\uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC785\
  \uB2C8\uB2E4\u2014\uBB38\uC81C\uB97C \uC77C\uC73C\uD0A4\uB294 \uC190\uB2D8\uB4E4\
  \uC744 \uCC98\uB9AC\uD558\uB294 \uBC14\uC6B4\uC11C\uCC98\uB7FC \uB9D0\uC774\uC8E0\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6D0\uD65C\uD55C \uAC83\uC744 \uC88B\
  \uC544\uD569\uB2C8\uB2E4; \uC5D0\uB7EC \uCC98\uB9AC\uB294 \uC608\uC0C1\uCE58 \uBABB\
  \uD55C \uC0C1\uD669\uC5D0 \uC9C1\uBA74\uD588\uC744 \uB54C \uCF54\uB4DC\uAC00 \uB118\
  \uC5B4\uC9C0\uACE0 \uC2E4\uC218\uD558\uB294 \uAC83\uC744 \uBC29\uC9C0\uD558\uC5EC\
  \ \uBB38\uC81C\uB97C \uD1B5\uC81C\uC5D0 \uB3C4\uC6C0\uC744 \uC90D\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.671745-06:00'
model: gpt-4-1106-preview
summary: "\uC5D0\uB7EC \uCC98\uB9AC\uB294 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uC608\
  \uC0C1\uCE58 \uBABB\uD55C \uC0C1\uD669\uC744 \uB2E4\uB8E8\uB294 \uAC83\uC785\uB2C8\
  \uB2E4\u2014\uBB38\uC81C\uB97C \uC77C\uC73C\uD0A4\uB294 \uC190\uB2D8\uB4E4\uC744\
  \ \uCC98\uB9AC\uD558\uB294 \uBC14\uC6B4\uC11C\uCC98\uB7FC \uB9D0\uC774\uC8E0. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6D0\uD65C\uD55C \uAC83\uC744 \uC88B\uC544\
  \uD569\uB2C8\uB2E4; \uC5D0\uB7EC \uCC98\uB9AC\uB294 \uC608\uC0C1\uCE58 \uBABB\uD55C\
  \ \uC0C1\uD669\uC5D0 \uC9C1\uBA74\uD588\uC744 \uB54C \uCF54\uB4DC\uAC00 \uB118\uC5B4\
  \uC9C0\uACE0 \uC2E4\uC218\uD558\uB294 \uAC83\uC744 \uBC29\uC9C0\uD558\uC5EC \uBB38\
  \uC81C\uB97C \uD1B5\uC81C\uC5D0 \uB3C4\uC6C0\uC744 \uC90D\uB2C8\uB2E4."
title: "\uC5D0\uB7EC \uCC98\uB9AC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
에러 처리는 프로그램에서 예상치 못한 상황을 다루는 것입니다—문제를 일으키는 손님들을 처리하는 바운서처럼 말이죠. 프로그래머들은 원활한 것을 좋아합니다; 에러 처리는 예상치 못한 상황에 직면했을 때 코드가 넘어지고 실수하는 것을 방지하여 문제를 통제에 도움을 줍니다.

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
