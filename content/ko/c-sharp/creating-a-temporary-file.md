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

## 무엇이고 왜?

임시 파일 생성이란 무엇인가? 임시 파일은 개발자들이 자주 사용하는 파일이며 일시적으로 사용하기 위해 만들어진 파일입니다. 프로그램이 실행되는 동안 필요한 정보를 저장하거나 처리할 수 있도록 할 때 유용하게 쓰입니다.

## 하는 법:

```C#
// 임시 파일을 생성하는 예제 코드
string tempFilePath = Path.GetTempFileName();
```

```C#
// 임시 파일을 생성하는 예제 코드 - 다른 디렉토리에 만드는 방법
string tempDirectory = @"C:\Temp";
string tempFilePath = Path.Combine(tempDirectory, Guid.NewGuid().ToString());
File.Create(tempFilePath);
```

위 코드를 실행하면 임시 파일이 생성되며, 해당 파일의 경로를 변수에 저장할 수 있습니다.

```C#
// 예제 코드 실행 결과
C:\Users\Username\AppData\Local\Temp\tmp123.tmp
```

## 깊게 들어가기:

임시 파일 생성은 운영 체제에서 제공하는 기능을 이용하는 것입니다. 예를 들어, Windows 운영 체제에서는 아래와 같이 `GetTempFileName` 함수를 제공합니다.

```C#
[DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
public static extern uint GetTempFileName(string tempDirectory, string prefix, uint unique, StringBuilder tempFileName);
```

또한, 사용자가 직접 임시 파일을 만드는 방법도 있습니다. 이 경우에는 무작위의 파일 이름을 생성하여 파일을 만드는데, GUID 값을 활용할 수 있습니다.

기존에 임시 파일을 만들어 사용한 후 삭제해야하지만, 최근에는 MemoryStream 클래스를 사용하여 임시 파일을 만들지 않고도 임시 저장 공간을 생성할 수 있습니다.

## 또 다른 방법이 궁금하신가요?

- [Temporary File 생성 방법 - Microsoft 기술문서](https://docs.microsoft.com/ko-kr/dotnet/standard/io/how-to-create-a-temporary-file)
- [C# 프로그램에서 임시 파일을 만드는 다른 방법 - 코드기억](http://www.codeengn.com/cs-how-to-create-temporary-file-in-csharp/)