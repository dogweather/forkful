---
title:    "C#: 현재 날짜 가져오기"
keywords: ["C#"]
---

{{< edit_this_page >}}

오늘 날짜를 얻는 것이 유용한 이유:

오늘 날짜를 얻는 것은 많은 프로그래밍 프로젝트에서 중요한 역할을 합니다. 예를 들어, 사용자가 앱 또는 웹 페이지에서 언제 무엇을 했는지 추적하려는 경우 오늘 날짜는 필수적입니다. 또한 매일 새로운 데이터를 저장하고 싶은 경우에도 오늘 날짜가 필요합니다. 따라서 오늘 날짜를 얻는 방법을 알고 있는 것은 프로그래밍에서 꼭 필요한 기술입니다.

어떻게 오늘 날짜를 얻을까요?

```C#
using System;

// 현재 날짜와 시간 출력
DateTime now = DateTime.Now;
Console.WriteLine(now);
```

위의 예시 코드에서 우리는 `DateTime` 데이터 타입을 사용하여 현재 날짜와 시간을 얻을 수 있습니다. 또한 `DateTime`의 다양한 속성을 사용하여 날짜와 시간을 자세하게 조작할 수 있습니다. 아래는 몇 가지 예시입니다.

```C#
// 현재 날짜 출력
DateTime today = DateTime.Today;
Console.WriteLine(today);

// 현재 시간 출력
DateTime now = DateTime.Now;
Console.WriteLine(now.ToShortTimeString());

// 오늘 날짜를 포맷에 맞게 출력
DateTime today = DateTime.Today;
Console.WriteLine(today.ToString("d"));
```

딥 다이브:

더 나아가서, 우리는 `DateTime`의 강력한 기능 중 하나인 `Parse()` 메서드를 활용할 수 있습니다. 이 메소드는 문자열을 `DateTime`으로 변환할 수 있도록 해줍니다. 아래는 이를 활용한 예시입니다.

```C#
// 2022년 1월 1일을 DateTime으로 변환
DateTime newYear = DateTime.Parse("1/1/2022");
Console.WriteLine(newYear);

// 사용자 입력을 DateTime으로 변환
Console.WriteLine("생년월일을 입력해주세요 (YYYY/MM/DD):");
string input = Console.ReadLine();
DateTime birthday = DateTime.Parse(input);
Console.WriteLine($"당신의 생일은 {birthday.ToString("D")} 입니다.");
```

강력한 `DateTime` 데이터 타입의 여러 가지 기능을 살펴봤습니다. 이를 이용하여 오늘 날짜를 자유롭게 조작할 수 있고, 여러분의 프로그래밍 프로젝트에 유용하게 활용할 수 있습니다.

See Also:

- [DateTime 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [DateTime.Now 속성 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.now?view=net-5.0)
- [DateTime.Parse 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.parse?view=net-5.0)