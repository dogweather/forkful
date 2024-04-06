---
date: 2024-01-20 18:03:10.746464-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) .NET Core CLI\uB294 2016\uB144\uC5D0 \uB4F1\
  \uC7A5, \uD50C\uB7AB\uD3FC \uAC04 \uAC1C\uBC1C\uC744 \uAC00\uB2A5\uCF00 \uD588\uC2B5\
  \uB2C8\uB2E4. Visual Studio\uC640 \uAC19\uC740 IDE\uB97C \uC0AC\uC6A9\uD558\uC9C0\
  \ \uC54A\uACE0\uB3C4 \uD504\uB85C\uC81D\uD2B8\uB97C \uC27D\uAC8C \uC2DC\uC791\uD560\
  \ \uC218 \uC788\uC8E0. `dotnet new` \uBA85\uB839\uC740 \uB2E4\uC591\uD55C \uD15C\
  \uD50C\uB9BF\uC744 \uC81C\uACF5\uD558\uC5EC \uC5EC\uB7EC \uC885\uB958\uC758\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.961993-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) .NET Core CLI\uB294 2016\uB144\uC5D0 \uB4F1\uC7A5\
  , \uD50C\uB7AB\uD3FC \uAC04 \uAC1C\uBC1C\uC744 \uAC00\uB2A5\uCF00 \uD588\uC2B5\uB2C8\
  \uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

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
