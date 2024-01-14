---
title:                "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지를 확인하는 작업을 왜 해야하는지에는 여러 가지 이유가 있습니다. 예를 들어, 코드 실행 중에 필요한 파일이나 데이터를 찾기 위해 디렉토리의 존재 여부를 확인하는 경우가 있을 수 있습니다. 또는 이미 존재하는 디렉토리를 생성하려는 경우 그 전에 디렉토리가 이미 존재하는지를 확인하는 것이 중요할 수 있습니다.

## 어떻게
디렉토리가 존재하는지 확인하는 방법은 간단합니다. 먼저, System.IO 네임스페이스를 사용하여 디렉토리 정보를 가져올 수 있습니다. 이후 Directory 클래스의 Exists 메소드를 사용하여 디렉토리가 존재하는지 여부를 확인할 수 있습니다.

```C#
using System;
using System.IO;

string path = @"C:\Users\user\Documents\Example Directory";

if (Directory.Exists(path))
{
    Console.WriteLine("디렉토리가 존재합니다.");
}
else
{
    Console.WriteLine("디렉토리가 존재하지 않습니다.");
}
```
출력:

```
디렉토리가 존재합니다.
```

## 딥 다이브
디렉토리가 존재하는지를 확인하는 방법에 대해 더 깊이 들어가 보겠습니다. Directory.Exists 메소드는 해당 디렉토리가 실제로 존재하는지를 확인하는 것이 아니라, 존재 여부를 확인할 수 있는 권한이 있는지를 확인합니다. 만약 디렉토리가 존재하지 않지만 해당 디렉토리를 읽기 권한이 있다면 Exists 메소드는 true를 반환합니다.

이를 방지하기 위해 Directory 클래스의 GetAccessControl 메소드와 FileSystemAccessRule 클래스를 사용하여 실제로 디렉토리가 존재하는지를 확인할 수 있습니다. 다음은 디렉토리의 읽기 권한이 있는지를 확인하는 예시 코드입니다.

```C#
using System;
using System.IO;
using System.Security.AccessControl;

string path = @"C:\Users\user\Documents\Example Directory";

if (Directory.Exists(path))
{
    DirectorySecurity directorySecurity = Directory.GetAccessControl(path);
    AuthorizationRuleCollection authorizationRules = directorySecurity.GetAccessRules(true, true, typeof(System.Security.Principal.NTAccount)); // NTAccount는 사용자를 나타내는 클래스
    bool hasReadPermission = false;

    foreach (FileSystemAccessRule fileSystemAccessRule in authorizationRules)
    {
        if (fileSystemAccessRule.FileSystemRights.HasFlag(FileSystemRights.Read) && fileSystemAccessRule.AccessControlType == AccessControlType.Allow)
        {
            hasReadPermission = true;
            break;
        }
    }

    if (hasReadPermission)
    {
        Console.WriteLine("디렉토리가 존재합니다.");
    }
    else
    {
        Console.WriteLine("디렉토리가 존재하지 않습니다.");
    }
}
else
{
    Console.WriteLine("디렉토리가 존재하지 않습니다.");
}
```

## 이어서 보기
디렉토리 관련 작업에 대해 깊이 알아보기 위해 아래 링크들을 참고해보세요.

- [C# 디렉토리 생성하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-create-a-directory)
- [C# 디렉토리 복사하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-copy-directories)
- [C# 디렉토리 삭제하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-delete-a-directory)
- [C# 디렉토리 정보 가져오기](https://docs.microsoft.com/ko-kr/dotnet