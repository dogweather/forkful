---
title:    "Gleam: 날짜를 문자열로 변환하기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜: 날짜를 문자열로 변환하는 이유
날짜를 문자열로 변환하는 것은 날짜 정보를 다루기 쉽고 가독성 좋게 표현하기 위해서입니다.

## 방법: "```Gleam ... ```" 코드 블록에 코딩 예제와 출력 결과

날짜를 문자열로 변환하는 방법은 다양합니다. 우선 `Date` 모듈을 임포트해야 합니다. 그리고 `date_to_string` 함수를 사용하여 날짜를 문자열로 변환할 수 있습니다.

```Gleam
import Date

let date = Date.new(year: 2021, month: 8, day: 15)

let date_string = Date.date_to_string(date, format: "%Y-%m-%d")
```

위의 예제에서는 `%Y-%m-%d` 형식으로 날짜를 문자열로 변환하였습니다. 이 코드를 실행하면 다음과 같은 출력 결과가 나옵니다.

```
"2021-08-15"
```

또 다른 형식인 `%B %d, %Y`을 사용하면 년-월-일이 아닌 월일년으로 표현할 수 있습니다. 이 외에도 다양한 형식을 사용할 수 있으며, `date_to_string` 함수의 두 번째 매개변수인 `format`으로 원하는 형식을 지정할 수 있습니다.

## 깊이 들어가기: 날짜를 문자열로 변환하는 과정

`Date` 모듈에서는 `date_to_string` 함수를 사용하여 날짜를 문자열로 변환합니다. 이 함수는 `format` 매개변수를 받아서 그 형식대로 날짜를 문자열로 변환해줍니다. 만약 형식이 지정되지 않으면 기본적인 `%m/%d/%Y` 형식으로 변환됩니다.

또한 `format` 매개변수에 `%F`를 사용하면 `%Y-%m-%d` 형식과 동일한 결과를 얻을 수 있습니다. 이는 `%Y-%m-%d` 형식을 축약하여 표현한 것입니다.

이 외에도 `Date` 모듈에는 날짜를 다양한 형식으로 표현하는 함수들이 있습니다. 이러한 함수들을 활용하면 날짜를 원하는 형식으로 변환할 수 있습니다.

## See Also (관련 링크)

- Gleam 공식 문서(https://gleam.run/documentation/)
- Gleam 커뮤니티 포럼(https://forum.gleam.run/)
- 날짜/시간 다루기 관련 기사(https://gleam.run/documentation/tutorials/dates-and-times/)