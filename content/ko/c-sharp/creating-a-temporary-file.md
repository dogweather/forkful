---
title:    "C#: 임시 파일 생성하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
일시적인 파일 생성을 하는 이유는 무엇일까요? 일시적인 파일은 프로그램에서 일시적으로 필요한 데이터를 저장하기 위해 사용됩니다. 프로그램이 실행 중에 생성되고 사용된 후에는 더 이상 필요하지 않으므로 삭제될 수 있습니다.

## 방법
일시적인 파일을 생성하는 방법은 다양합니다. 아래의 예시와 같이 C#에서 파일을 생성하고 삭제하는 간단한 코드를 살펴보겠습니다.

```C#
// 임시 파일의 경로를 지정합니다.
string tempFilePath = @"C:\Temp\tempfile.txt";

// 임시 파일을 생성합니다.
File.Create(tempFilePath);

// 생성된 임시 파일을 삭제합니다.
File.Delete(tempFilePath);

// 생성된 임시 파일이 존재하는지 확인합니다.
// 출력 결과: False
Console.WriteLine(File.Exists(tempFilePath));
```

위의 코드는 단순한 예시이며, 실제로는 더 복잡한 로직이 필요할 수 있습니다. 또한 임시 파일의 경로와 파일명은 각각의 프로그램과 용도에 따라 달라질 수 있습니다.

## 깊이 파고들기
임시 파일은 실제로는 디렉토리에 생성됩니다. 일반적으로는 시스템의 임시 디렉토리에 저장되며, 이는 "Temp"나 "Temporay"과 같은 폴더 이름을 갖고 있습니다. 이 디렉토리를 방문하면 프로그램에서 생성한 임시 파일뿐만 아니라 시스템에서 생성한 임시 파일도 함께 볼 수 있습니다. 임시 파일을 삭제할 때, 프로그램에서 생성한 임시 파일만을 삭제해야 한다는 점을 주의해야 합니다. 그렇지 않으면 시스템의 임시 파일까지 함께 삭제할 수 있으며, 이는 시스템에 영향을 미칠 수 있습니다.

## 그 외 참고 자료
- [C#에서 파일 생성하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-create-a-file)
- [C#에서 파일 삭제하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-delete-a-file)
- [시스템 임시 파일 디렉토리에 대한 MSDN 문서](https://docs.microsoft.com/ko-kr/windows/win32/api/winbase/nf-winbase-gettemppathw)
- [.NET Framework에서 임시 디렉토리 관리하기](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-manage-temporary-files-and-directories-in-net)