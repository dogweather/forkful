---
title:    "C#: 텍스트 검색 및 대체"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

텍스트 검색 및 대체를 사용해야 하는 이유는 간단합니다. 이 기술은 대량의 텍스트를 다루는 프로그램에 꼭 필요하기 때문입니다. 예를 들어, 여러 개의 파일에서 특정 문자열을 찾아서 다른 문자열로 대체하거나, 특정 패턴에 맞는 문자열을 찾아서 다른 패턴으로 변경하는 등의 작업을 할 때 유용하게 사용할 수 있습니다.

# 사용 방법

텍스트 검색 및 대체를 코드로 구현하는 방법을 알아보겠습니다. 먼저, ```C# regex``` 패키지를 참조해야 합니다. 그리고 아래의 예시 코드를 통해 텍스트를 검색하고 대체하는 과정을 살펴보겠습니다.

```C#
using System;
using System.Text.RegularExpressions;

namespace TextSearchAndReplace
{
    class Program
    {
        static void Main(string[] args)
        {
            // 텍스트 검색과 대체를 위한 정규식 패턴 설정
            string pattern = @"cat";
            string replacement = "dog";

            // 검색하고 대체할 텍스트가 담긴 문자열
            string input = "I have a cat and a dog.";

            // 패턴과 대체문자열을 이용해 정규식 객체 생성
            Regex regex = new Regex(pattern);

            // 대체된 결과 출력
            string result = regex.Replace(input, replacement);
            Console.WriteLine(result);
        }
    }
}
```

위의 예시 코드의 실행 결과는 다음과 같습니다.

```
I have a dog and a dog.
```

# 깊이 파고들기

텍스트 검색 및 대체 기능을 사용할 때 유용하게 활용할 수 있는 다양한 특수 문자와 정규식 메타 문자가 있습니다. 이를 이용하면 더 복잡한 패턴을 적용하고 텍스트를 더 다양한 방식으로 검색 및 대체할 수 있습니다. 또한, 다양한 패턴을 조합해 사용하는 것도 가능합니다.

예를 들어, 여러 개의 파일에서 "dog"로 시작하는 문자열을 찾아서 "cat"으로 대체한다면 아래와 같은 정규식 패턴을 사용할 수 있습니다.

```
^dog
```

위의 패턴은 주어진 텍스트에서 "dog"로 시작하는 문자열을 찾아내기 때문에 여러 개의 파일에서 해당 패턴에 맞는 문자열을 찾아서 대체하는 것이 가능해집니다.

또한, 정규식을 사용하면 대/소문자 구분 없이 검색할 수 있는 옵션도 제공됩니다. 예를 들어, "Cat"이나 "cat" 또는 "CAT" 모두를 찾고 싶을 때는 아래와 같은 옵션을 추가하면 됩니다.

```
Regex regex = new Regex(pattern, RegexOptions.IgnoreCase);
```

이 외에도 정규식을 이용해 다양한 검색 및 대체 작업을 할 수 있고, 조금 더 깊숙한 지식이 필요한 경우에는 정규식에 대해 더 공부할 수 있습니다.

# 참고 자료

- [정규식 사용 방법](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [정규식 온라인 도구](https://regexr.com/)
- [정규식 챌린지 사이트](https://regexone.com/)