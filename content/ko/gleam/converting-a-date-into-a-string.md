---
title:    "Gleam: 날짜를 문자열로 변환하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 이유는 때때로 프로그래밍에서 필요한 작업입니다. 예를 들어, 사용자가 생일을 입력했을 때, 이를 보기 좋은 형식으로 출력하거나 파일 이름에 날짜를 포함하여 정렬하는 등의 경우에 필요할 수 있습니다.

## 어떻게

```Gleam
import gleam/date
date |> date.to_string() // Sample Output: "2021-08-18T23:59:59Z"
```

날짜를 문자열로 변환하기 위해서는 먼저 Gleam의 date 모듈을 import해야 합니다. 그리고 date.to_string() 메서드를 사용하여 원하는 날짜를 문자열로 변환할 수 있습니다. 위 예시에서는 현재 날짜와 시간이 "yyyy-mm-ddThh:mm:ssZ" 형식으로 출력되는 것을 볼 수 있습니다.

## 깊이 분석

날짜를 문자열로 변환하는 과정에서는 다양한 포맷 옵션이 존재합니다. Gleam의 date 모듈에는 여러가지 미리 정의된 포맷이 있으며, 추가적으로 사용자가 직접 포맷을 지정할 수도 있습니다. 예를 들어, ```date.to_string("{YYYY}.{MM}.{DD}")```과 같이 사용하여 "yyyy.mm.dd" 형식으로 출력할 수 있습니다.

또한 날짜와 관련된 다른 유용한 모듈들도 있습니다. 예를 들어, 날짜 간의 차이나 날짜를 기반으로 새로운 날짜를 계산하는 등의 작업을 할 수 있는 Date.Comparison, Date.Arithmetic 모듈이 있습니다.

## 참고자료

- Gleam 공식 문서: https://gleam.run/
- Gleam에서 날짜 다루기: https://gleam.run/documentation/standard-libraries/date