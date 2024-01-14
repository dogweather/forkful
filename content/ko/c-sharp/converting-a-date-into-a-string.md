---
title:    "C#: 날짜를 문자열로 변환하기."
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜:

사람들이 날짜를 문자열로 변환하는 이유는 유용한 프로그래밍 기능 중 하나입니다. 문자열로 변환된 날짜를 사용하면 프로그램에서 날짜를 쉽게 출력할 수 있으며, 사용자에게 날짜 형식을 자유롭게 선택할 수 있도록 합니다.

## 어떻게:

날짜를 문자열로 변환하는 방법은 간단합니다. 먼저 ```C# DateTime```을 사용하여 날짜를 입력받아 변수에 저장합니다. 그런 다음 ```C# .ToString()``` 메서드를 사용하여 지정된 형식으로 날짜를 문자열로 변환합니다. 아래는 날짜를 문자열로 변환하는 예제 코드입니다.

```C#
// 현재 날짜를 저장하는 변수
DateTime now = DateTime.Now;

// yyyy/MM/dd 형식으로 날짜를 문자열로 변환
string dateAsString = now.ToString("yyyy/MM/dd");

// 출력 결과: 2021/07/20
Console.WriteLine(dateAsString);
```

위의 예제 코드에서 ```now.ToString("yyyy/MM/dd")``` 부분에서 ```yyyy/MM/dd```는 원하는 날짜 형식을 지정할 수 있습니다. 이를 다른 형식으로 바꾸면 다른 형태의 문자열로 변환될 수 있습니다. 예를 들어 ```now.ToString("MM/dd/yyyy")```는 07/20/2021과 같은 형식으로 날짜를 출력할 것입니다.

## 깊게 파헤치기:

실제로 날짜를 문자열로 변환하는 프로세스는 영어로 된 표현이 나오기 쉽습니다. 하지만 ```C#```은 언어적 설정을 바꿀 수 있기 때문에 다양한 언어에서 동작할 수 있습니다. 또한 ```C# .ToString()``` 메서드는 다양한 기능을 제공하며 원하는 형식으로 날짜를 변환할 수 있도록 합니다. 예를 들어 특정 문화권에서 흔히 사용하는 날짜 형식을 변환하거나 날짜와 시간을 모두 출력하는 것도 가능합니다.

## 참고자료:

- [C# 날짜를 문자열로 변환하는 방법](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/how-to-convert-a-string-to-a-datetime)
- [C# .ToString() 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.tostring?view=net-5.0)