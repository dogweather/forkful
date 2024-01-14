---
title:    "C#: 디렉토리가 존재하는지 확인하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일 및 디렉토리를 다루는 프로그램을 개발할 때, 특정 디렉토리가 존재하는지 확인해야 할 때가 있습니다. 이를 위해 C# 프로그래밍에서는 디렉토리가 존재하는지 확인하는 방법을 제공합니다.

## 방법

```C#
using System;
using System.IO;

namespace CheckDirectoryExistence
{
    class Program
    {
        static void Main(string[] args)
        {
            // 존재하는 디렉토리 경로 설정
            string existingDirectory = @"C:\Users\TestFolder";
            
            // 디렉토리가 존재하는지 확인
            if (Directory.Exists(existingDirectory))
            {
                Console.WriteLine("디렉토리가 존재합니다.");
            }
            else
            {
                Console.WriteLine("디렉토리가 존재하지 않습니다.");
            }
            
            // 존재하지 않는 디렉토리 경로 설정
            string nonExistingDirectory = @"C:\Users\NonExistingFolder";
            
            // 디렉토리가 존재하는지 확인
            if (Directory.Exists(nonExistingDirectory))
            {
                Console.WriteLine("디렉토리가 존재합니다.");
            }
            else
            {
                Console.WriteLine("디렉토리가 존재하지 않습니다.");
            }
        }
    }
}
```

출력:

```
디렉토리가 존재합니다.
디렉토리가 존재하지 않습니다.
```

## 딥 다이브

C#에서 디렉토리가 존재하는지 확인하는 방법은 `Directory.Exists()` 메서드를 사용하는 것입니다. 이 메서드는 `bool` 타입을 반환하며, 디렉토리가 존재하면 `true`를, 존재하지 않으면 `false`를 반환합니다.

추가적으로, `Directory.Exists()` 메서드는 매개변수로 디렉토리 경로를 문자열로 받아야 합니다. 이 문자열은 절대 경로이어야 하며, 특정 디렉토리에 대한 상대 경로를 사용할 수도 있습니다.

## 참고자료

- [Microsoft Docs: Directory.Exists 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory.exists)
- [C# Directory.Exists Example](https://www.codota.com/code/c-sharp/methods/System.IO.Directory.Exists)
- [Directory.Exists vs. File.Exists in C#](https://stackoverflow.com/questions/3235609/directory-exists-vs-file-exists-in-c-sharp)
- [C# 파일 및 디렉토리 다루기](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/file-system/)