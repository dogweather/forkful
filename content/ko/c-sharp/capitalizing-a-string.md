---
title:    "C#: 문자열 대문자로 변환하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

텍스트를 대문자로 변환하는 것은 프로그래밍에서 자주 사용되는 기능 중 하나입니다. 이 기능을 통해 문자열을 일관성 있게 표현할 수 있으며, 데이터 처리와 문자열 조작에 유용합니다.

## 어떻게

```C#
string text = "hello world!";

Console.WriteLine(text.ToUpper());
// Output: HELLO WORLD!
```

```C#
string text = "hello world!";

Console.WriteLine(text.ToLower());
// Output: hello world!
```

위의 예제는 C#에서 문자열을 대문자 또는 소문자로 변환하는 방법을 보여줍니다. 첫 번째 예제에서는 `ToUpper()` 메소드를 사용하여 문자열을 모두 대문자로 변환하고, 두 번째 예제에서는 `ToLower()` 메소드를 사용하여 문자열을 모두 소문자로 변환합니다.

또한 `CultureInfo` 클래스를 사용하여 언어 및 지역 설정에 따라 문자열을 변환할 수도 있습니다. 예를 들어, 다양한 언어에서 "i"를 대문자로 변환할 때 "İ"를 대문자로 변환시키는 기능을 사용할 수 있습니다.

```C#
string text = "i love programming";

var cultureInfo = new CultureInfo("tr-TR");
Console.WriteLine(cultureInfo.TextInfo.ToUpper(text));
// Output: İ LOVE PROGRAMMING
```

이렇게 하면 "i"대신 "İ"가 대문자로 변환됩니다.

## 심층 고찰

문자열 변환은 간단한 작업처럼 보일 수 있지만, 내부적으로는 많은 로직이 들어가 있습니다. 예를 들어, 영문 소문자를 대문자로 변환할 때는 단순히 ASCII 코드만 변환하면 되지만, 다른 언어에서는 문자 간 관계 및 언어 규칙을 고려해야 합니다. 이러한 사항을 고려하여 개발자는 문자열 변환 기능을 어떻게 구현할지 결정할 수 있습니다.

## 관련 자료

- [The Power of String.ConvertToCulture(string) Method in C#](https://www.oreilly.com/library/view/c-cookbook-4th/9781492071695/ch04s05.html)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [String Operations in C#](https://www.educative.io/edpresso/string-operations-in-c-sharp)