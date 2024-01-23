---
title:                "텍스트 검색 및 교체"
date:                  2024-01-20T17:57:46.933963-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 검색 및 교체"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색 및 교체는 문자열에서 콘텐츠를 찾아 다른 콘텐츠로 바꾸는 과정입니다. 프로그래머들은 데이터 수정, 코드 리팩토링, 혹은 사용자 입력 처리 시 이 작업을 자주 수행합니다.

## How to: (어떻게?)
```C#
using System;

class Program
{
    static void Main()
    {
        string originalText = "안녕하세요, 프로그래밍 세계!";
        string searchText = "세계";
        string replaceText = "우주";

        string updatedText = originalText.Replace(searchText, replaceText);

        Console.WriteLine("변경 전: " + originalText);
        Console.WriteLine("변경 후: " + updatedText);
    }
}

// 샘플 출력:
// 변경 전: 안녕하세요, 프로그래밍 세계!
// 변경 후: 안녕하세요, 프로그래밍 우주!
```

## Deep Dive (심층 분석)
텍스트 검색 및 교체는 초기 컴퓨팅 시절부터 있었던 작업입니다. 초기 텍스트 편집기들에서 기본적인 명령어이기도 했죠. C#에서 `String.Replace` 메서드는 가장 일반적인 방법입니다.

`Regex.Replace`는 또 다른 방법입니다. 이는 복잡한 패턴 일치가 필요할 때 유용합니다. 예를 들어, 특정 단어 앞뒤의 공백, 혹은 특정 형식의 전화번호 패턴을 찾아 교체할 수 있습니다.

교체 로직은 글로벌(global) 혹은 첫 번째 일치만(local) 찾을지 선택가능합니다. `Replace`는 기본적으로 모든 일치 항목을 교체하지만, `Regex`를 사용하면 더 세밀한 제어가 가능합니다.

## See Also (참고 자료)
- [Microsoft's String.Replace Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Microsoft's Regex.Replace Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- [Stack Overflow: Replace vs Regex.Replace in C#](https://stackoverflow.com/questions/6275980/string-replace-vs-regex-replace)

이글을 읽고 텍스트 검색 및 교체 방법을 이해하는 데 도움이 되었기를 바랍니다. 더 배우고 싶다면 위의 참고 자료 링크들을 확인해 보세요.
