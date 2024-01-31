---
title:                "디렉토리의 존재 여부 확인하기"
date:                  2024-01-19
html_title:           "Arduino: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"

category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리가 존재하는지 확인하는 것은 파일 시스템에 특정 폴더가 있는지 알아보는 과정입니다. 프로그래머들은 파일을 저장하거나 읽기 전에 에러를 방지하기 위해 이 작업을 합니다.

## How to: (어떻게 할까요?)
C#에서 디렉토리가 존재하는지 알아보는 가장 흔한 방법은 `System.IO` 네임스페이스 안에 있는 `Directory.Exists` 메소드를 사용하는 것입니다. 존재 여부에 따라 불린값을 반환합니다.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = @"C:\example";

        if (Directory.Exists(path))
        {
            Console.WriteLine("Directory exists.");
        }
        else
        {
            Console.WriteLine("Directory does not exist.");
        }
    }
}
```
출력 예시:
```
Directory exists.
```
또는
```
Directory does not exist.
```

## Deep Dive (심층 분석)
`Directory.Exists` 메소드는 .NET Framework의 초창기부터 사용되어 왔습니다. 이는 IO 작업을 간단히 처리하기 위해 설계된 방법이며, 단순함과 강력함을 동시에 제공합니다. 대안으로 파일 시스템에 접근할 때 `FileInfo`나 `DirectoryInfo` 클래스를 사용할 수도 있지만, 그 경우 더 많은 정보를 얻을 수 있지만 성능이 조금 떨어질 수 있습니다. 존재 여부만 확인하려면 `Directory.Exists`가 가장 직관적입니다. 이 메소드는 내부적으로는 `FindFirstFile` 윈도우 API를 호출하여 작동합니다.

## See Also (참고 자료)
- .NET API Documentation for `Directory.Exists`: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- MSDN magazine article about file system manipulation: [MSDN Magazine](https://msdn.microsoft.com/magazine/mt842503)
