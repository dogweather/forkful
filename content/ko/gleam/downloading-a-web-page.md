---
title:                "웹 페이지 다운로드"
html_title:           "Gleam: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 이게 뭐고 왜 하는 거야?:
웹 페이지를 다운로드한다는 것이 무엇인지 알 수 있는가? 개발자들은 왜 이런 작업을 하는지 궁금하지 않은가?

## 하는 방법:
```Gleam ... ``` 코드 블록 안에 코딩 예제와 출력 결과를 제공한다.

```Gleam
// 예제 1: Gleam의 내장 함수인 httpc를 사용하여 웹 페이지 다운로드
response := httpc.get("https://www.example.com")
html := String.from_bytes(response.body)
```
출력 결과:
```
<!doctype html>
<html>
  <head>
    <title>Example Domain</title>
    ...
```

```Gleam
// 예제 2: Gleam의 extlib를 사용하여 웹 페이지 다운로드
response := extlib.httpc_get("https://www.example.com")
html := String.from_bytes(response.body)
```
출력 결과:
```
<!doctype html>
<html>
  <head>
    <title>Example Domain</title>
    ...
```

## 깊이 파헤치기:
(1) 역사적 배경, (2) 대안, (3) 웹 페이지 다운로드의 구현 세부사항과 같은 깊이 있는 정보를 제공한다.
- Gleam은 Erlang 가상 머신 위에서 동작하는 병렬성과 상호작용성에 강점이 있는 함수형 프로그래밍 언어이다. 그래서 웹 요청과 응답을 처리하는데 있어서도 뛰어난 성능을 발휘한다.
- 다운로드 외에도 웹 페이지를 표시하는 방법으로는 Scrapping, Crawling과 같은 기술이 있다. 이러한 기술들은 큰 규모의 데이터를 수집하거나 웹 페이지의 특정 컨텐츠를 추출하는 데에 사용된다.
- Gleam에서 웹 페이지를 다운로드하는 방법은 여러 가지가 있지만 가장 간단한 방법은 httpc를 사용하는 것이다. 이 외에도 extlib를 이용하거나 Rust 라이브러리인 reqwest를 사용할 수도 있다.

## 관련 자료:
관련 자료를 참고할 수 있는 링크를 제공한다.
- [Gleam 공식 홈페이지](http://www.gleam.run/)
- [Gleam의 HTTP 클라이언트 문서](http://gleam.run/stdlib/Http.Client.html)
- [Scrapping과 Crawling의 차이점에 대한 설명](https://www.scrapehero.com/scraping-vs-crawling-whats-difference/)