---
title:                "C#: 패턴에 맞는 문자 삭제하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

컴퓨터 프로그래밍을 하다보면, 우리는 종종 데이터 정리를 위해 특정한 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이러한 상황에서 우리는 어떻게 해야 할지 궁금할 수 있습니다. 그래서 오늘은 C#에서 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보겠습니다.

## 어떻게 해야 할까요?

우선, 우리는 특정 문자열 안에 있는 패턴과 일치하는 문자를 모두 삭제해야 합니다. 이를 위해 우리는 C#에서 제공하는 `Regex` 클래스를 사용할 수 있습니다. `Regex` 클래스는 정규식을 사용하여 문자열을 분석하고 처리하는데 사용됩니다.

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
	public static void Main()
	{
		// 문자열 입력
		string str = "Hello World! 1234";
		
		// 삭제할 패턴 정의
		Regex regex = new Regex("[0-9]"); // 숫자를 가진 문자
		
		// Replace 메서드를 사용하여 패턴과 일치하는 문자를 공백으로 대체
		str = regex.Replace(str, "");
		
		// 결과 출력
		Console.WriteLine(str); // 결과: Hello World!
	}
}
```

위의 예시에서 보듯이, `Regex` 클래스의 `Replace` 메서드를 사용하여 패턴과 일치하는 문자를 제거할 수 있습니다. 이렇게 하면 문자열에서 원하는 부분만 남기고 나머지는 모두 삭제할 수 있습니다. 추가적으로 `Regex` 클래스를 사용하면 정규식을 이용해 더 다양한 패턴과 일치하는 문자를 삭제할 수 있습니다.

## 딥 다이브

보다 깊은 수준에서 패턴과 일치하는 문자를 삭제하는 것은 정규식과 관련된 지식이 필요합니다. 정규식은 문자 패턴을 표현하는 방식을 나타내는데, 예를 들어 `[0-9]`는 0부터 9까지의 숫자를 의미합니다. 이렇게 정규식으로 패턴을 정의하면, 해당하는 문자를 찾아서 원하는 처리를 할 수 있습니다.

하지만 정규식은 처음에 배우기에는 조금 난해할 수 있지만, 능숙하게 다룰 수 있다면 매우 유용한 도구가 될 것입니다. 자세한 내용은 링크를 참고하시기 바랍니다.

## 같이 보기

- [C#에서 정규식 사용하기](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)