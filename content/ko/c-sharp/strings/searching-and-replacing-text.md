---
date: 2024-01-20 17:57:46.933963-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C?) \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F\
  \ \uAD50\uCCB4\uB294 \uCD08\uAE30 \uCEF4\uD4E8\uD305 \uC2DC\uC808\uBD80\uD130 \uC788\
  \uC5C8\uB358 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uCD08\uAE30 \uD14D\uC2A4\uD2B8 \uD3B8\
  \uC9D1\uAE30\uB4E4\uC5D0\uC11C \uAE30\uBCF8\uC801\uC778 \uBA85\uB839\uC5B4\uC774\
  \uAE30\uB3C4 \uD588\uC8E0. C#\uC5D0\uC11C `String.Replace` \uBA54\uC11C\uB4DC\uB294\
  \ \uAC00\uC7A5 \uC77C\uBC18\uC801\uC778 \uBC29\uBC95\uC785\uB2C8\uB2E4. `Regex.Replace`\uB294\
  \ \uB610 \uB2E4\uB978 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uC774\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.944752-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C?) \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\
  \uB294 \uCD08\uAE30 \uCEF4\uD4E8\uD305 \uC2DC\uC808\uBD80\uD130 \uC788\uC5C8\uB358\
  \ \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

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
