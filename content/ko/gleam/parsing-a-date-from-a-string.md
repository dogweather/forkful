---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
날짜를 문자열에서 파싱한다는 것은 무엇인가요? 그리고 프로그래머들이 왜 이렇게 하나요?

날짜 파싱은 날짜를 특정한 형식으로 변환하는 것을 말합니다. 예를 들어, "2021-05-17"라는 문자열을 2021년 5월 17일로 변환하는 것입니다. 프로그래머들은 이를 통해 날짜를 더 쉽게 다룰 수 있으며, 데이터베이스나 파일에서 날짜를 정렬하거나 비교하는 등의 작업을 수행할 수 있습니다.

# How to:

```Gleam 
import Time
import Time.Parse

Time.Parse.date("2021-05-17")
|> case Ok(date) -> date // 2021-05-17
  Err(err) -> panic(err)
```

위의 예제는 "2021-05-17"라는 문자열을 날짜로 파싱하는 방법을 보여줍니다. 먼저 Time 모듈과 Time.Parse 모듈을 불러온 다음, Time.Parse.date 함수를 사용하여 해당 문자열을 날짜로 변환합니다. 변환된 날짜를 Ok(date)로 받아서 사용하거나, 변환에 실패할 경우 Err(err)를 반환합니다.

# Deep Dive
날짜 파싱은 오래된 컴퓨터 시스템에서부터 사용된 기술이며, 데이터베이스나 파일에서 날짜를 처리하기 위해 많이 사용됩니다. 하지만 최근에는 이러한 작업을 라이브러리나 프레임워크가 대신 해주는 경우가 많습니다. 예를 들어, JavaScript에서는 moment.js와 같은 라이브러리를 사용하여 날짜를 파싱하고 변환할 수 있습니다. Gleam에서는 Time.Parse 라이브러리를 사용하여 쉽게 날짜 파싱을 할 수 있습니다.

# See Also
- [Gleam 공식 문서](https://gleam.run/documentation/) - Gleam 언어와 관련된 공식 문서를 확인할 수 있습니다.
- [Gleam에 대한 인터뷰](https://gleam.run/blog/interviews/2020-03-13-interviews-with-gleam-community.html) - Gleam 커뮤니티 회원들과의 인터뷰를 통해 Gleam에 대해 더 많이 알아볼 수 있습니다.
- [moment.js 공식 사이트](https://momentjs.com/) - JavaScript에서 널리 사용되는 날짜 처리를 위한 라이브러리입니다.