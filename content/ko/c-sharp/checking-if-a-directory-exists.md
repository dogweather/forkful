---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 & 왜?

디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 특정 위치에 디렉토리가 있음을 검증하는 것입니다. 이는 파일 및 데이터 저장을 안전하게 운영하고, 에러를 방지하기 위해 프로그래머들이 자주 사용합니다.

## 어떻게:

디렉토리가 있는지 확인하는 방법을 C# 코드로 보여드리겠습니다.

```C#
using System.IO;

class Program
{
    static void Main()
    {
        var dirPath = @"C:\MyDirectory";

        if (Directory.Exists(dirPath))
        {
            System.Console.WriteLine("Directory exists.");
        }
        else
        {
            System.Console.WriteLine("Directory does not exist.");
        }
    }
}
```
이 코드에서, 디렉토리가 있으면 "Directory exists."라는 메시지가 표시되고, 없으면 "Directory does not exist."라는 메시지가 표시됩니다.

## 깊은 이해:

디렉토리가 존재하는지 확인하는 최초의 방법이나 기능은 파일 시스템이 소프트웨어에 도입되었을 때 나왔습니다. 그래서 이것은 파일 시스템 이해의 기초적인 부분입니다. 

대안으로, `DirectoryInfo.Exists`메서드도 사용되곤 합니다. 하지만, `Directory.Exists`는 static 메서드이므로 더 쉽게 접근할 수 있습니다.

`Directory.Exists` 메서드는 디렉토리 경로를 문자열로 받아, 그 경로에 디렉토리가 있으면 `true`를, 없으면 `false`를 반환합니다. 이 결과는 애플리케이션의 로직을 결정하는데 도움이 됩니다.

## 관련 자료

- MSDN 문서: [Directory.Exists Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- StackOverflow: [Check if directory exists](https://stackoverflow.com/questions/1395205/better-way-to-check-if-a-path-is-a-file-or-a-directory)
- C# 공식 문서: [File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)