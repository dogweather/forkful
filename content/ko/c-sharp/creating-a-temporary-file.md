---
title:                "C#: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

하는 이유

이 글에서는 C# 프로그래밍을 하고 있는 분들을 위해 임시 파일을 생성하는 방법을 알려드리겠습니다. 임시 파일은 프로그램에서 가끔 필요한 경우가 있습니다. 예를 들어 사용이 끝나면 삭제하는 임시 데이터를 저장하는 등의 상황에서 사용될 수 있습니다.

How To:

우선 임시 파일을 생성하기 위해서는 System.IO 패키지를 사용해야 합니다. 아래의 예제와 같이 코드를 작성해야 합니다.

```C#
using System;
using System.IO;

namespace TempFileExample
{
    class Program
    {
        static void Main(string[] args)
        {
            // 임시 파일 생성
            string tempFilePath = Path.GetTempFileName();
            Console.WriteLine("임시 파일 생성: {0}", tempFilePath);
            
            // 임시 파일에 데이터 쓰기
            using (StreamWriter writer = new StreamWriter(tempFilePath))
            {
                writer.WriteLine("임시 데이터 쓰기 테스트");
            }

            // 임시 파일 읽기
            using (StreamReader reader = new StreamReader(tempFilePath))
            {
                Console.WriteLine("임시 파일 데이터: {0}", reader.ReadToEnd());
            }

            // 임시 파일 삭제
            if (File.Exists(tempFilePath))
            {
                File.Delete(tempFilePath);
                Console.WriteLine("임시 파일 삭제 완료");
            }
        }
    }
}
```

위의 코드를 실행하면 아래의 결과가 나옵니다.

```C#
임시 파일 생성: C:\Users\UserName\AppData\Local\Temp\temp5738.tmp
임시 파일 데이터: 임시 데이터 쓰기 테스트
임시 파일 삭제 완료
```

Deep Dive:

위의 예제에서는 Path.GetTempFileName() 메소드를 사용하여 임시 파일을 생성했습니다. 이 메소드는 운영 체제에서 고유한 임시 파일 이름을 생성하고, 이후에는 이 파일을 읽고 쓸 수 있도록 FileStream을 반환합니다.

또한 StreamWriter 클래스를 사용하여 파일에 데이터를 쓴 후, StreamReader 클래스를 사용하여 데이터를 읽고 출력하였습니다. 마지막으로, 파일이 사용되지 않을 경우 삭제하기 위해 File.Delete() 메소드를 사용하였습니다.

See Also:

- https://docs.microsoft.com/en-us/dotnet/api/system.io.file.create
- https://docs.microsoft.com/en-us/dotnet/api/system.io.file.delete
- https://docs.microsoft.com/en-us/dotnet/api/system.io.filestream
- https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter
- https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader