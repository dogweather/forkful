---
title:                "C#: 디렉토리가 존재하는지 확인하는 방법"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 왜?

파일 또는 디렉토리를 만들 때 항상 있는지 없는지를 확인해야 합니다. 그렇기 때문에 코딩하는 도중에도 디렉토리가 존재하는지 확인해야 할 때가 있을 수 있습니다.

# 어떻게 확인할까요?

디렉토리가 존재하는지 확인하는 것은 C#에서 아주 간단한 작업입니다. DirectoryInfo 개체를 사용하면 됩니다.

```C#
if (Directory.Exists(@"C:\Temp\Documents"))
{
    Console.WriteLine("The directory exists!");
}
else
{
    Console.WriteLine("The directory doesn't exist.");
}
```

위의 예제 코드를 실행시키면 "The directory exists!"라는 메시지가 출력될 것입니다.

디렉토리가 존재하지 않는 경우에는 "The directory doesn't exist."라는 메시지가 출력됩니다.

# 더 들어가기

디렉토리가 존재하는지 확인하는 더 많은 방법이 있습니다. 예를 들어, 디렉토리가 존재하는지 확인할 때 디렉토리의 속성을 가져오는 것도 가능합니다.

```C#
DirectoryInfo dir = new DirectoryInfo(@"C:\Temp\Documents");
FileAttributes attributes = dir.Attributes;

if ((attributes & FileAttributes.Directory) == FileAttributes.Directory)
{
    Console.WriteLine("The directory exists!");
}
else
{
    Console.WriteLine("The directory doesn't exist.");
}
```

출력 결과는 이전과 동일할 것입니다.

# 더 알아보기

디렉토리를 만들 때 디렉토리가 이미 존재하는지 확인하는 것은 중요한 작업입니다. 따라서 C#에서 디렉토리를 다루는 더 자세한 내용을 배우는 것이 좋습니다. [이 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory?view=net-5.0)는 Directory 클래스에 대한 자세한 정보를 제공합니다.

# 더 보기

- [Directory.Exists 메서드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory.exists?view=net-5.0)
- [DirectoryInfo 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directoryinfo?view=net-5.0)
- [FileAttributes 열거형 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.fileattributes?view=net-5.0)