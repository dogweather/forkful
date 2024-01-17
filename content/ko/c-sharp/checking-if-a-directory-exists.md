---
title:                "디렉터리가 존재하는지 확인하기"
html_title:           "C#: 디렉터리가 존재하는지 확인하기"
simple_title:         "디렉터리가 존재하는지 확인하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 무엇과 왜?

디렉토리가 존재하는지를 확인하는 것은 프로그래머들이 자주 하는 작업입니다. 이는 프로그램 로직에서 필수적인 부분이며, 파일들을 찾거나 생성하기 전에 확인해야 합니다.

# 방법:

```C#
if (Directory.Exists(path))
{
    Console.WriteLine("디렉토리가 존재합니다.");
}
else
{
    Console.WriteLine("디렉토리가 존재하지 않습니다.");
}
```

이 코드 예제는 ```path``` 변수에 지정된 경로에 디렉토리가 있는지 여부를 확인합니다. ```Directory.Exists()``` 메서드는 불리언 값으로 결과를 반환하며, 디렉토리가 존재하면 ```true```를 반환하고 그렇지 않으면 ```false```를 반환합니다.

# 깊이 들어가기:

디렉토리의 존재 여부를 확인하는 것은 프로그래밍에서 매우 중요한 부분입니다. 이는 예외처리와 관련이 있으며, 없는 디렉토리를 쿼리하면 오류가 발생할 수 있습니다. 이를 방지하기 위해 디렉토리가 존재하는지 먼저 확인해야 합니다.

```Directory.Exists()``` 메서드는 존재 여부 뿐만 아니라 디렉토리의 속성도 확인할 수 있습니다. 이를 사용하여 디렉토리가 읽기 전용인지, 숨김 파일이 있는지 등을 확인할 수 있습니다.

대안으로는 ```DirectoryInfo``` 클래스를 사용할 수 있습니다. 이 클래스는 디렉토리의 존재 여부를 확인할 수 있는 다양한 메서드와 속성을 제공합니다.

# 참고 자료:

- C# 가이드: 디렉토리 쿼리 - https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/file-system/how-to-query-directories
- C# 가이드: 디렉토리 정보 - https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/file-system/how-to-get-information-about-a-file-path