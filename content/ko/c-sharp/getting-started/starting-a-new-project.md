---
title:                "새 프로젝트 시작하기"
date:                  2024-01-20T18:03:10.746464-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜 인가?)
새 프로젝트 시작하기는 빈 캔버스에 그림을 그리는 것 같아요. 새로운 아이디어를 실현하거나 문제를 해결하기 위해 프로그래머들은 프로젝트를 시작합니다.

## How to: (어떻게:)
```C#
// .NET Core CLI를 사용하여 새 콘솔 앱 생성
dotnet new console -o MyNewApp
cd MyNewApp

// Program.cs에 간단한 'Hello World' 코드 삽입
using System;

namespace MyNewApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("안녕, 새 프로젝트!");
        }
    }
}

// 애플리케이션 실행
dotnet run
```
출력:
```
안녕, 새 프로젝트!
```

## Deep Dive (심층 해석)
.NET Core CLI는 2016년에 등장, 플랫폼 간 개발을 가능케 했습니다. Visual Studio와 같은 IDE를 사용하지 않고도 프로젝트를 쉽게 시작할 수 있죠. `dotnet new` 명령은 다양한 템플릿을 제공하여 여러 종류의 프로젝트를 시작할 수 있도록 합니다. `dotnet new console`은 콘솔 애플리케이션을 생성합니다. 앞으로는 더욱 복잡하고 기능적인 앱을 구축하면서 프로젝트 구성과 패키지 관리의 이해가 필요합니다.

## See Also (참고 자료)
- [.NET CLI 개요](https://docs.microsoft.com/ko-kr/dotnet/core/tools/)
- [Visual Studio 시작 가이드](https://docs.microsoft.com/ko-kr/visualstudio/get-started/csharp/tutorial-console?view=vs-2022)
- [C# 가이드](https://docs.microsoft.com/ko-kr/dotnet/csharp/)
- [.NET 프로젝트 구조](https://docs.microsoft.com/ko-kr/dotnet/core/project-sdk/overview)
