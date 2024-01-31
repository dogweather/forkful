---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
html_title:           "Arduino: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)

문자열을 대문자로 만든다는 것은 문자열 속 모든 글자를 대문자 형태로 변환하는 것을 말합니다. 프로그래머들은 일관성 있게 데이터를 표시하거나 사용자에게 보일 때 중요한 부분을 강조하기 위해 이를 사용합니다.

## How to: (어떻게 할까?)

C#에는 문자열을 대문자로 쉽게 바꿀 수 있는 메서드들이 있습니다. 예시를 통해 살펴봅시다.

```C#
using System;

class CapitalizeString
{
    static void Main()
    {
        var example = "hello, world!";
        var capitalizedExample = example.ToUpper();
        Console.WriteLine(capitalizedExample);
    }
}
```

출력:
```
HELLO, WORLD!
```

## Deep Dive (심층 분석)

대문자 변환은 프로그래밍 언어가 발전하면서 도입되었습니다. C#의 경우 `.ToUpper()`는 간단하게 모든 소문자를 대문자로 변환해 줄 수 있는 메서드입니다. 이 메서드는 문화권과 관련된 문자에 대응하여 `.ToUpperInvariant()` 같은 대안도 제공합니다. 문자열을 대문자로 변환 시 내부적으로는 각 문자의 유니코드 값을 참조하여 대응하는 대문자로 매핑합니다.

## See Also (참고 자료)

- [Microsoft Documentation on ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Microsoft Documentation on ToUpperInvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
- [Unicode Standard](https://home.unicode.org/)
