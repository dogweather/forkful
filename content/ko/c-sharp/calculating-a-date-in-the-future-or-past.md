---
title:    "C#: 미래나 과거의 날짜 계산하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜
날짜를 미래 또는 과거로 계산하는 것에 참여하고 싶은 이유는 무엇인가?
날짜를 계산하는 것은 많은 프로그래밍 작업에서 필수적입니다. 예를 들어, 거래 기록 분석, 이벤트 일정, 또는 간단한 유효 기간 계산 등 다양한 상황에서 날짜 계산이 필요합니다. 또한 모바일 앱이나 웹 서비스에서 날짜를 표시하는데도 사용될 수 있으므로, 날짜 계산에 대한 이해는 향후 프로그래밍 프로젝트에서 큰 도움이 될 것입니다.

## 어떻게
"```C#
// 현재 날짜로부터 3일 후의 날짜 계산하는 예시
DateTime currentDate = DateTime.Now;
DateTime futureDate = currentDate.AddDays(3);
Console.WriteLine("3일 후의 날짜는: " + futureDate.ToString());
```
위의 코드에서는 C#의 DateTime 클래스를 사용해 날짜를 계산하는 방법을 보여줍니다. "AddDays" 메서드를 사용해 현재 날짜에 원하는 일 수 만큼을 더하여 미래 날짜를 계산할 수 있습니다. 또한 ToString() 메서드를 사용해 날짜를 원하는 형식으로 출력할 수 있습니다.

"```C#
// 입력한 이전 날짜에 대해 특정 일 수를 더하여 역으로 계산하는 예시
Console.WriteLine("과거 날짜를 입력하세요. (MM/dd/yyyy): ");
DateTime pastDate = DateTime.Parse(Console.ReadLine());
Console.WriteLine("과거 날짜로부터 10일 후의 날짜는: " + pastDate.AddDays(-10).ToString());
```
위의 코드에서는 사용자로부터 입력받은 이전 날짜를 기준으로 AddDays 메서드를 사용해 역으로 계산한 예시를 보여줍니다. 여기서 AddDays의 매개변수에 음수 값을 전달하여 과거 날짜로부터 역으로 계산할 수 있습니다.

## 딥 다이브
DateTime 클래스에는 대부분의 날짜 계산에 필요한 다양한 메서드가 있습니다. AddMonths, AddYears, AddHours 등의 메서드를 사용해 날짜를 계산할 수 있습니다. 또한 날짜와 관련된 다양한 정보를 가져오는 메서드도 있습니다. 예를 들어, DayOfWeek 메서드를 이용해 특정 날짜의 요일을 알아올 수 있습니다. 이러한 DateTime 클래스의 메서드를 잘 활용하면 더욱 복잡한 날짜 계산도 쉽고 간단하게 할 수 있습니다.

# 참고
- [DateTime.AddXXX 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.addmonths?view=netcore-3.1)
- [DateTime 클래스 예제와 설명](https://www.csharpstudy.com/Tip/Tip-datetime.aspx)
- [C#에서 날짜와 시간 다루기](https://zetawiki.com/wiki/C-Date)
- [C# 날짜와 시간 다루기 총정리](https://shoark7.github.io/programming/csharp/datetime-timezone-part-one/)