---
title:    "Gleam: 현재 날짜 얻기"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# 왜 
오늘 날짜를 얻는 것에 대해 관심이 있는 이유는 무엇일까요? 코드에서 현재 날짜를 사용하면 날짜와 시간을 기록하거나 특정 작업의 유효 기간을 확인할 수 있기 때문입니다.

## 방법 
"```Gleam
import Time

let date = Time.now()
```"
위의 코드는 Gleam에서 현재 날짜를 얻기위한 간단한 예제입니다. 여러분은 언제든지 이 코드를 사용하여 현재 날짜를 파악할 수 있습니다. 코드의 실행 결과는 다음과 같습니다.

"```Gleam
2021-07-21T15:46:33.595500Z
```"

## 깊게 파고들기 
현재 날짜를 얻기 위해서는 Gleam의 Time 라이브러리를 사용해야 합니다. 이 라이브러리에는 날짜와 시간을 다루는 다양한 함수와 유용한 기능이 있습니다. 예를 들어, `now()` 함수는 UTC 형식의 현재 날짜와 시간을 반환합니다. 또한 `format()` 함수를 사용하면 원하는 형식으로 날짜를 표시할 수 있습니다.

# 참고 자료 
- [Gleam 공식 문서 - Time 라이브러리](https://gleam.run/libraries/time/) 
- [Gleam 공식 문서 - 현재 날짜와 시간 얻기](https://gleam.run/articles/getting-current-date-time/)
- [Gleam 커뮤니티 포럼 - 날짜 및 시간 표시 방법](https://forum.gleam.run/t/634)