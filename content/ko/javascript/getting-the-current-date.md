---
title:                "Javascript: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜 Javascript로 오늘 날짜를 가져오는가?

자바스크립트를 사용하여 현재 날짜를 가져오는 것은 많은 이유가 있습니다. 가장 일반적인 이유는 웹 개발에서 날짜가 필요한 경우입니다. 예를 들어, 사용자가 언제 가입했는지를 알려주는 등의 목적으로 사용할 수 있습니다. 또한 생산성 도구로 날짜를 자동으로 기록하는 등 다양한 사용 용도가 있을 수 있습니다.

## 방법

날짜를 가져오는 방법은 매우 간단합니다. `Date()` 객체를 사용하여 현재 날짜와 시간을 가져올 수 있습니다. 아래는 예제 코드와 그에 해당하는 출력입니다.

```Javascript
let date = new Date();

console.log(date);
```

```
Thu Oct 14 2021 15:48:23 GMT+0900 (대한민국 표준시)
```

`Date()` 객체는 자바스크립트에서 기본 제공되는 객체이므로 추가적으로 라이브러리를 설치할 필요 없이 바로 사용할 수 있습니다.

## 심층 분석

`Date()` 객체의 출력과 같이 표준 시간대가 포함되어 있기 때문에 특정 지역의 시간을 가져오려면 추가적인 작업이 필요합니다. `toLocaleString()` 메소드를 사용하여 원하는 형식으로 날짜를 변환할 수 있습니다. 아래는 코드와 출력 예시입니다.

```Javascript
let date = new Date();

console.log(date.toLocaleString('en-US', {hour12: true}));
```

```
10/14/2021, 3:51:56 PM
```

그리고 `Date()` 객체는 컴퓨터의 로컬 시간을 기준으로 날짜를 가져오기 때문에 사용자의 지역과 다른 시간이 출력될 수 있습니다. 이를 해결하기 위해서는 Javascript의 `Intl` 객체와 `Intl.DateTimeFormat` 메소드를 사용하여 사용자의 지역을 파악하고 그에 맞는 날짜를 출력할 수 있습니다. 아래는 코드와 출력 예시입니다.

```Javascript
let date = new Date();

console.log(new Intl.DateTimeFormat('ko-KR', {year: 'numeric', month: 'long', day: 'numeric', weekday: 'long', timeZone: 'Asia/Seoul'}).format(date));
```

```
2021년 10월 14일 목요일, 한국 표준시
```

# 더 알아보기

더 자세한 정보를 알고 싶다면 아래 링크를 참고해보세요.

[Date() 객체 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)

[Intl 객체 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Intl)

[Intl.DateTimeFormat() 메소드 공식 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/DateTimeFormat)