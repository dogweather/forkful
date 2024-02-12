---
title:                "텍스트 파일 쓰기"
date:                  2024-02-03T19:27:42.921967-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
