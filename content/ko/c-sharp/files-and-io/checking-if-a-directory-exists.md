---
title:                "디렉토리가 존재하는지 확인하기"
date:                  2024-02-03T19:07:30.242073-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

C#에서 디렉토리 존재 여부 확인은 파일 시스템의 지정된 경로에 폴더의 존재를 검증하는 것을 포함합니다. 프로그래머들은 존재하지 않는 디렉토리에서 읽거나 쓰려고 시도하는 등의 오류를 피하기 위해 이러한 검사를 수행하며, 이를 통해 파일 및 디렉토리 조작을 더욱 원활하게 만듭니다.

## 방법:

### System.IO 사용하기

C#은 `System.IO` 네임스페이스를 제공하며, 이 네임스페이스는 `Directory` 클래스를 포함하여 디렉토리 존재 여부를 직접 확인할 수 있는 `Exists` 메소드를 제공합니다.

```csharp
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\ExampleDirectory";

        // 디렉토리가 존재하는지 확인
        bool directoryExists = Directory.Exists(directoryPath);

        // 결과 출력
        Console.WriteLine("디렉토리 존재 여부: " + directoryExists);
    }
}
```

**샘플 출력:**

```
디렉토리 존재 여부: False
```

`C:\ExampleDirectory` 경로에 디렉토리가 실제로 존재하는 경우, 출력은 `True`가 됩니다.

### 단위 테스트를 위한 System.IO.Abstractions 사용하기

파일 시스템과 상호 작용할 때 코드를 단위 테스트할 수 있게 만드는 것에 관한 한, `System.IO.Abstractions` 패키지는 인기 있는 선택입니다. 이를 통해 테스트에서 파일 시스템 작업을 추상화하고 모방할 수 있습니다. 다음은 이러한 접근 방식을 사용하여 디렉토리의 존재 여부를 확인하는 방법입니다:

먼저, 패키지를 설치했는지 확인하세요:

```
Install-Package System.IO.Abstractions
```

그 다음, 클래스에 `IFileSystem`을 주입하고 이를 사용하여 디렉토리가 존재하는지 확인할 수 있습니다. 이를 통해 단위 테스트가 용이해집니다.

```csharp
using System;
using System.IO.Abstractions;

class Program
{
    private readonly IFileSystem _fileSystem;

    public Program(IFileSystem fileSystem)
    {
        _fileSystem = fileSystem;
    }

    public bool CheckDirectoryExists(string directoryPath)
    {
        return _fileSystem.Directory.Exists(directoryPath);
    }

    static void Main()
    {
        var fileSystem = new FileSystem();
        var program = new Program(fileSystem);

        string directoryPath = @"C:\ExampleDirectory";
        bool directoryExists = program.CheckDirectoryExists(directoryPath);

        Console.WriteLine("디렉토리 존재 여부: " + directoryExists);
    }
}
```

**샘플 출력:**

```
디렉토리 존재 여부: False
```

이 접근법은 애플리케이션 로직을 직접적인 파일 시스템 접근에서 분리함으로써 코드를 더 모듈화되고, 테스트 가능하며, 유지보수하기 쉽게 만듭니다.