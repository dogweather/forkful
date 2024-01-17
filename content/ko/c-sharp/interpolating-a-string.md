---
title:                "문자열 보간"
html_title:           "C#: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

보간된 문자열이 무엇인지 알고싶나요? 그렇다면 이 기사가 바로 당신에게 필요한 정보를 제공할 겁니다! 보간된 문자열들은 C#에서 사용되는 강력한 기능 중 하나로서 코드를 간결하게 만들어주고, 변화하는 데이터를 쉽게 포매팅할 수 있게 해줍니다. 이제 이것을 도와주는 이 방법을 알아보도록 할게요.

## 방법:

```C#
string name = "John";
string message = $"Hello {name}, welcome to our website!";
Console.WriteLine(message);
```
위의 코드를 실행하면 "Hello John, welcome to our website!"라는 메세지가 출력될 거에요. 보간된 문자열을 사용하면 따로 포매팅 함수를 쓰지 않아도, 중괄호 안에 넣은 변수의 값을 직접 출력할 수 있어요. 또한, 문자열 안에 표현식이나 메소드 호출 등 다양한 기능을 사용할 수 있기 때문에 코드를 더 간결하게 만들 수 있어요.

## 깊이 파고들기:

보간된 문자열은 2015년 출시된 C# 6.0에서 새롭게 도입된 기능입니다. 이전 버전의 C#에서는 String.Format()이나 + 연산자를 사용해 문자열을 포매팅해야 했는데, 이것으로 인해 코드가 복잡해지고 가독성이 떨어졌습니다. 하지만 보간된 문자열을 사용하면 코드의 가독성과 유지보수가 훨씬 쉬워집니다.

보간된 문자열의 다른 대안으로는 String interpolation이 있습니다. 이 방법은 문자열 앞에 $ 기호 대신 $ 참조 연산자를 사용한다는 점을 제외하면 위의 예제와 거의 동일합니다. 또한, 보간된 문자열의 내부 동작 방식을 알아보면, 중괄호 안에 표현식이나 메소드 호출을 넣을 수 있는 이유는 실제로 String.Format()을 이용해 문자열을 만드는 것과 같다는 것을 알 수 있습니다.

## 관련 자료:

보간된 문자열에 대해 더 자세한 정보를 원하신다면 아래의 링크를 참고해보세요! 이 기사에서 당신은 더 이상 보간된 문자열이 무엇인지 궁금해할 필요가 없을 거에요.

- [Microsoft Docs: 문자열 보간 - C# 프로그래밍 가이드](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/tokens/interpolated)
- [제이 블로그(Jay Blog) - 테이블 순환이 앞뒤 순환이 아닌 경우, 보간 문자열 사용하기](https://www.jay.blog/395)