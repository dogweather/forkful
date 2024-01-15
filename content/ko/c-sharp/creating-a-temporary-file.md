---
title:                "임시 파일 만들기"
html_title:           "C#: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

잠시 사용되는 파일을 만드는 이유는 여러 가지가 있습니다. 가장 일반적인 이유는 프로그램이 특정 작업을 수행하는 동안 일시적으로 데이터를 저장할 필요가 있기 때문입니다. 또는 프로그램 종료 시 데이터를 삭제하고 메모리를 확보하기 위해 임시 파일을 사용할 수도 있습니다.

## 만드는 방법

여기서는 C#의 `Path.GetTempFileName()` 메서드를 사용하여 임시 파일을 만드는 방법을 알아보겠습니다.

```
C# 
using System;
using System.IO;

namespace TemporaryFileExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // 임시 파일 이름 생성
            string tempFileName = Path.GetTempFileName();

            // 임시 파일 생성
            using (var tempFile = File.Create(tempFileName))
            {
                // 임시 파일에 데이터 쓰기
                using (var writer = new StreamWriter(tempFile))
                {
                    writer.WriteLine("Hello world!");
                }
            }

            // 임시 파일 삭제
            File.Delete(tempFileName);

            Console.WriteLine("임시 파일이 생성되었습니다.");
            Console.ReadLine();
        }
    }
}
```

위 코드를 실행하면 `C:\Users\사용자이름\AppData\Local\Temp` 폴더에 임시 파일이 생성되고, 해당 파일에 "Hello world!"가 쓰여집니다. 프로그램이 종료되면 임시 파일은 자동으로 삭제됩니다.

## 딥 다이브

`Path.GetTempFileName()` 메서드는 실제로 `GetTempFileNameW()` Win32 API를 호출하여 임시 파일을 만듭니다. 이 함수는 파일 이름으로 사용할 고유한 임시 파일 이름을 생성하고, 해당 파일을 생성한 다음 파일 핸들을 반환합니다. `using`키워드를 사용하면 파일 핸들을 자동으로 닫고, `File.Delete()` 메서드를 사용하여 임시 파일을 삭제할 수 있습니다.

## 관련 링크

- [Microsoft Docs - Path.GetTempFileName 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.path.gettempfilename)
- [Microsoft Docs - GetTempFileNameW 함수](https://docs.microsoft.com/ko-kr/windows/win32/api/fileapi/nf-fileapi-gettempfilenamew)