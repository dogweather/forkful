---
title:                "문자열에서 날짜를 구분하기"
html_title:           "C#: 문자열에서 날짜를 구분하기"
simple_title:         "문자열에서 날짜를 구분하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

무엇과 왜?

(1) 날짜를 문자열에서 추출하는 것이 무엇인지 고민해보세요. 이것은 컴퓨터에게 우리가 읽고 이해할 수 있는 형태로된 일련의 문자열을 제공하는 것입니다. 이를 컴퓨터가 이해할 수 있는 방식으로 변환해야하며, 이는 필수적인 작업입니다.

(2) 프로그래머들은 이 작업을 수행하는 이유는 매우 간단합니다. 데이터가 문자열 형태로 저장되어 있기 때문입니다. 그러나 우리는 이 데이터를 표준화된 형태로서 사용하고 싶습니다. 예를 들어, 사용자가 입력한 날짜를 문서에 표시할 때, 우리는 컴퓨터가 이를 이해할 수 있는 형식으로 표현하고 싶은 것입니다.

어떻게:

(parsing a date from a string 예시)

```c#
var stringDate = "2020-10-31"; // 문자열에서 추출할 날짜
var date = DateTime.Parse(stringDate); // Parse 메서드를 사용하여 추출
Console.WriteLine(date); // 정상적으로 추출된 날짜를 출력
```

출력:

```c#
10/31/2020 12:00:00 AM
```

깊이 탐색:

(1) 이 작업은 컴퓨터 분야에서 현재도 많이 사용되는 기술입니다. 예전에는 매우 복잡한 작업이었지만, 현재는 좀 더 쉽게 처리할 수 있습니다.

(2) 날짜를 문자열 형태로 저장할 때에는 꼭 위 예시와 같은 형식("YYYY-MM-DD")으로 저장하는 것을 추천합니다. 또한 ParseExact 메서드를 사용하여 특정한 형식으로 저장된 날짜를 추출할 수도 있습니다.

(3) C#에서는 Parsing을 위해 Parse, ParseExact, TryParse 등의 메서드가 제공됩니다. 각 메서드는 다른 방식으로 동작하므로 필요에 따라 알맞은 메서드를 사용해야합니다. 또한, 친절한 오류 메시지가 표시되므로 포맷이 잘못되었을 때도 쉽게 이해할 수 있습니다.

참고자료:

- https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.parse?view=netcore-3.1
- https://www.c-sharpcorner.com/blogs/parse-datetime-using-c-sharp1
- https://www.tutorialsteacher.com/csharp/csharp-datetime
- https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.tryparse?view=netcore-3.1