---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:42.921967-07:00
description: "\uC5B4\uB5BB\uAC8C: C#\uC740 `System.IO` \uB124\uC784\uC2A4\uD398\uC774\
  \uC2A4\uB97C \uD1B5\uD574 \uD30C\uC77C \uC791\uC5C5\uC744 \uAC04\uC18C\uD654\uD558\
  \uBA70, \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\uB294 \uBC29\uBC95\
  \uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uC5EC\uAE30\uC5D0 \uAE30\uBCF8 \uD14D\uC2A4\
  \uD2B8 \uD30C\uC77C\uC744 \uC791\uC131\uD558\uACE0 \uAE30\uC874 \uD30C\uC77C\uC5D0\
  \ \uD14D\uC2A4\uD2B8\uB97C \uCD94\uAC00\uD558\uB294 \uBC29\uBC95\uC774 \uC788\uC2B5\
  \uB2C8\uB2E4. #."
lastmod: '2024-03-13T22:44:55.261731-06:00'
model: gpt-4-0125-preview
summary: "C#\uC740 `System.IO` \uB124\uC784\uC2A4\uD398\uC774\uC2A4\uB97C \uD1B5\uD574\
  \ \uD30C\uC77C \uC791\uC5C5\uC744 \uAC04\uC18C\uD654\uD558\uBA70, \uD14D\uC2A4\uD2B8\
  \ \uD30C\uC77C\uC744 \uC791\uC131\uD558\uB294 \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 어떻게:
C#은 `System.IO` 네임스페이스를 통해 파일 작업을 간소화하며, 텍스트 파일을 작성하는 방법을 제공합니다. 여기에 기본 텍스트 파일을 작성하고 기존 파일에 텍스트를 추가하는 방법이 있습니다.

### 처음부터 텍스트 파일에 쓰기
```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "Hello, world!";

        // 새 파일에 내용 쓰기
        File.WriteAllText(filePath, content);
        
        Console.WriteLine("파일에 성공적으로 쓰였습니다.");
    }
}
```
**_sample_output_**
```
파일에 성공적으로 쓰였습니다.
```

### 기존 파일에 텍스트 추가
기존 파일 끝에 텍스트를 추가하고 싶다면 `File.AppendAllText` 메소드를 사용할 수 있습니다.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string additionalContent = "\n더 많은 내용 추가.";

        // 파일에 내용 추가
        File.AppendAllText(filePath, additionalContent);
        
        Console.WriteLine("내용이 성공적으로 추가되었습니다.");
    }
}
```
**_sample_output_**
```
내용이 성공적으로 추가되었습니다.
```

### 제3자 라이브러리 사용하기: `StreamWriter`
자동 플러싱과 인코딩 선택을 포함하여 쓰기를 보다 세밀하게 제어하기 위해서는 `StreamWriter`를 사용하세요.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\example\ExampleFile.txt";
        string content = "StreamWriter를 사용한 예시입니다.";

        // StreamWriter를 사용하여 파일에 쓰기
        using (StreamWriter writer = new StreamWriter(filePath, append: true))
        {
            writer.WriteLine(content);
        }
        
        Console.WriteLine("StreamWriter로 파일 쓰기가 성공적으로 되었습니다.");
    }
}
```
**_sample_output_**
```
StreamWriter로 파일 쓰기가 성공적으로 되었습니다.
```

이러한 각 접근법은 서로 다른 필요성을 충족시킵니다: 빠른 작업을 위한 직접 `File` 방법과 보다 복잡한 쓰기 시나리오를 위한 `StreamWriter`. 성능과 파일 크기와 같은 요소들을 고려하며 구체적인 요구 사항에 따라 선택하세요.
