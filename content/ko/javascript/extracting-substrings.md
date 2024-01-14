---
title:    "Javascript: 부분 문자열 추출"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 왜 substring을 추출하는지

substring 추출은 문자열을 다루는 프로그래밍에서 매우 일반적인 작업입니다. 우리가 substring을 추출하는 이유는 여러 가지가 있을 수 있지만, 가장 일반적인 이유는 문자열을 조작하고 특정 부분만을 사용하고 싶을 때입니다. 예를 들어, 전화번호에서 지역번호만을 추출하거나, 이메일 주소에서 도메인만을 추출할 수 있습니다.

# substring 추출하는 방법

만약 여러분이 문자열을 다루는 프로그래밍을 하고 있다면, substring 추출은 언젠가는 반드시 사용하게 될 것입니다. 이제부터는 실제로 substring을 추출하는 방법에 대해 알아보겠습니다.

## 기본 문법

여러분은 다음과 같은 방법으로 문자열의 substring을 추출할 수 있습니다.

```Javascript
let str = "Hello, World!";
let substr = str.substring(0, 5); // "Hello"
```

위의 코드에서는 `substring` 메소드를 사용하여 문자열 `str`의 인덱스 0부터 5 이전까지의 부분 문자열을 `substr`에 저장하고 있습니다. 중간에 `substring` 메소드의 파라미터로 들어가는 숫자들은 `start`와 `end` 인덱스를 나타냅니다. 여기서 `start`는 추출하려는 부분 문자열의 시작 인덱스를, `end`는 추출하려는 부분 문자열의 마지막 인덱스보다 1 큰 값을 나타냅니다.

## 응용 예제

이제 실제로 substring을 추출해보는 예제를 살펴보겠습니다.

```Javascript
let phoneNum = "010-1234-5678";
let areaCode = phoneNum.substring(0, 3); // "010"
```

위의 코드에서는 `-`을 구분자로 사용하여 전화번호에서 지역번호만을 추출하고 있습니다. 또 다른 예제를 살펴보면,

```Javascript
let email = "example@mail.com";
let domain = email.substring(7, 15); // "mail.com"
```

이번에는 `@`을 구분자로 사용하여 이메일 주소에서 도메인만을 추출하고 있습니다.

# substring에 대해 더 알아보기

지금까지 substring을 추출하는 기본적인 방법과 응용 예제를 살펴보았습니다. 하지만 substring에 대해 아직 모르는 것이 있다면 걱정하지 마세요. 이제 더 깊이 들어가서 substring을 추출하는 데 도움이 되는 몇 가지 정보들을 알아보겠습니다.

## 인덱스 값

substring 추출 시 주의해야 할 점 중 하나는 인덱스 값의 범위입니다. 위에서 살펴본 것처럼 `substring` 메소드의 파라미터로 들어가는 `start`와 `end` 값은 추출하려는 문자열의 인덱스를 나타냅니다. 따라서 `start`는 항상 `end`보다 작아야 합니다. 그렇지 않으면 원하는 부분 문자열을 제대로 추출할 수 없을 뿐 아니라 오류가 발생할 수도 있습니다.

## 음수 인덱스

substring 추출 시 음수 인덱스를 사용할 수도 있습니다. 음수 인덱스를 사용하게 되면 뒤에서부터 인덱싱을 하게 됩니다. 예를 들어,

```Javascript
let str = "Hello, World!";
let substr = str.substring(-3, -1); // "ld"
```

위의 코드에서는 `start