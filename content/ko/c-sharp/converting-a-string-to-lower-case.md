---
title:    "C#: 문자열을 소문자로 변환하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

여러분은 C# 프로그래밍을 하다가 문자열을 소문자로 바꾸는 작업이 있을 때가 있을 것입니다. 그래서 오늘은 왜 문자열을 소문자로 변환해야 하는지, 그리고 어떻게 소문자로 바꿀 수 있는지 알아보려고 합니다.

## 왜

문자열을 소문자로 바꾸는 이유는 여러 가지가 있지만, 가장 일반적인 이유는 데이터의 통일성을 유지하기 위해서입니다. 예를 들어, 사용자의 이름을 입력받을 때 대문자와 소문자가 섞여있으면 같은 이름임에도 다른 데이터로 인식할 수 있습니다. 이런 경우 모두 소문자로 바꾸어서 처리하는 것이 좋습니다.

## 어떻게

C#에서는 문자열을 소문자로 바꾸는 간단한 메소드를 제공하고 있습니다. 바로 `ToLower()` 메소드입니다. 이 메소드를 사용하면 소문자로 변환된 문자열을 리턴합니다. 예제를 통해 살펴보겠습니다.

```C#
string name = "John";
string lowerCaseName = name.ToLower();
Console.WriteLine(lowerCaseName);
```

이 코드의 결과는 다음과 같이 나옵니다.

```
john
```

문자열 `name`이 `"John"`이었지만 `ToLower()` 메소드를 사용하여 소문자로 변환된 `"john"`을 리턴합니다. 이제 모든 문자열이 소문자로 통일되어 있어서 데이터를 처리하기가 더 쉬워졌습니다.

## Deep Dive

C#의 `ToLower()` 메소드는 매우 간단하지만 조금 더 깊이 들어가 보겠습니다. 이 메소드는 `.NET Framework`의 `String` 클래스에 속해 있습니다. 이 클래스는 매우 유용한 메소드들을 많이 가지고 있어서 문자열을 다룰 때 매우 편리합니다. `ToLower()` 메소드만을 사용하는 것이 아니라, 다른 메소드들도 함께 사용하면 더욱 다양한 작업을 할 수 있습니다. `String` 클래스에 대해서는 나중에 더 자세하게 살펴보도록 하겠습니다.

## See Also

- C# String 클래스 : https://docs.microsoft.com/ko-kr/dotnet/api/system.string?view=netcore-3.1
- .NET Framework String 클래스 : https://docs.microsoft.com/ko-kr/dotnet/api/system.string?view=netcore-3.1

이제 여러분은 C#에서 문자열을 소문자로 바꾸는 작업을 어떻게 할 수 있는지 알게 되었습니다. `ToLower()` 메소드를 사용하면 간단하게 소문자로 변환할 수 있습니다. 더 자세한 정보가 필요하다면 `String` 클래스를 살펴보세요. 감사합니다.