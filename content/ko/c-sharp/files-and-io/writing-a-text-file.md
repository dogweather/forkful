---
aliases:
- /ko/c-sharp/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:42.921967-07:00
description: "C#\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC791\uC131\uC740 \uD504\
  \uB85C\uADF8\uB798\uBC0D \uBC29\uC2DD\uC73C\uB85C \uD30C\uC77C \uC2DC\uC2A4\uD15C\
  \uC0C1\uC5D0 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098\
  \ \uC218\uC815\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB294 \uB85C\
  \uAE45, \uB370\uC774\uD130 \uB0B4\uBCF4\uB0B4\uAE30, \uC124\uC815 \uAD00\uB9AC \uB4F1\
  \ \uB9CE\uC740 \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uAE30\uBCF8\uC801\
  \uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC138\
  \uC158 \uAC04\uC5D0 \uB370\uC774\uD130\uB97C \uC720\uC9C0\uD558\uAC70\uB098 \uC2DC\
  \uC2A4\uD15C \uAC04\uC5D0 \uC815\uBCF4\uB97C\u2026"
lastmod: 2024-02-18 23:09:06.249336
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC791\uC131\uC740 \uD504\
  \uB85C\uADF8\uB798\uBC0D \uBC29\uC2DD\uC73C\uB85C \uD30C\uC77C \uC2DC\uC2A4\uD15C\
  \uC0C1\uC5D0 \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098\
  \ \uC218\uC815\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB294 \uB85C\
  \uAE45, \uB370\uC774\uD130 \uB0B4\uBCF4\uB0B4\uAE30, \uC124\uC815 \uAD00\uB9AC \uB4F1\
  \ \uB9CE\uC740 \uC751\uC6A9 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C \uAE30\uBCF8\uC801\
  \uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC138\
  \uC158 \uAC04\uC5D0 \uB370\uC774\uD130\uB97C \uC720\uC9C0\uD558\uAC70\uB098 \uC2DC\
  \uC2A4\uD15C \uAC04\uC5D0 \uC815\uBCF4\uB97C\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C#에서 텍스트 파일 작성은 프로그래밍 방식으로 파일 시스템상에 텍스트 파일을 생성하거나 수정하는 것을 말합니다. 이는 로깅, 데이터 내보내기, 설정 관리 등 많은 응용 프로그램에서 기본적인 작업입니다. 프로그래머는 세션 간에 데이터를 유지하거나 시스템 간에 정보를 공유하거나, 사람이 읽을 수 있는 출력을 저장하기 위해 이 작업을 수행합니다.

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
