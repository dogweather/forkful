---
title:                "C#: 텍스트 파일 작성하기"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

이 포스트는 텍스트 파일을 작성하는 방법에 대해 알고 싶은 독자들을 위한 것입니다. 만약 당신이 개발자라면 이미 텍스트 파일을 작성하는 데에 대한 필요성을 금방 알 수 있을 것입니다.

## 왜

텍스트 파일은 소프트웨어 개발에서 매우 중요한 요소입니다. 우리는 코드를 작성하고, 문서를 작성하고, 데이터를 저장하며, 협업을 합니다. 모두 텍스트 파일을 통해 이루어집니다. 그래서 텍스트 파일을 작성하는 것은 당연한 일입니다.

## 어떻게

텍스트 파일을 작성하는 가장 간단한 방법은 C#의 내장 클래스 중 하나인 `StreamWriter`를 사용하는 것입니다. 이 클래스는 파일을 열고 쓰기를 위한 메서드를 제공합니다. 아래는 예제 코드입니다.

```C#
using (StreamWriter sw = new StreamWriter("sample.txt"))
{
    sw.WriteLine("This is a sample text file.");
}
```

이 코드는 `"sample.txt"`라는 이름의 파일을 만들고 그 안에 `"This is a sample text file."`라는 텍스트를 쓰는 것입니다. 실행하면 `sample.txt` 파일이 생성되고, 그 안에는 해당 텍스트가 적혀있을 것입니다.

또한, 파일에 계속해서 쓰는 것이 아니라 한 번에 여러 줄의 텍스트를 쓰고 싶다면 `WriteAllLines()` 메서드를 사용할 수도 있습니다.

```C#
string[] lines = { "This is line 1.", "This is line 2.", "This is line 3." };
File.WriteAllLines("sample.txt", lines);
```

이 코드를 실행하면 `sample.txt`에는 각 줄마다 각기 다른 텍스트가 적혀있을 것입니다.

## 깊이 파고들기

`StreamWriter` 클래스는 여러 가지 다른 설정을 변경할 수 있으며, 텍스트 파일을 쓰기 전 후에도 여러 작업을 할 수 있습니다. 예를 들어, 파일을 쓴 후에는 파일을 닫아주는 것이 좋습니다. 그렇지 않으면 메모리 누수가 발생할 수 있습니다. `using` 문을 사용하면 자동으로 파일이 닫히므로 편리합니다.

또한, C#에는 파일 경로를 다루는 여러 가지 기능이 있습니다. 예를 들어, 디렉토리의 파일 목록을 가져올 수 있는 `Directory.GetFiles()` 메서드를 사용할 수 있습니다. 더 많은 기능을 알고 싶다면 MSDN 문서를 참조하세요.

## 또 다른 자료

* [StreamWriter 클래스 MSDN 문서](https://msdn.microsoft.com/ko-kr/library/system.io.streamwriter(v=vs.110).aspx)
* [Directory 클래스 MSDN 문서](https://msdn.microsoft.com/ko-kr/library/system.io.directory(v=vs.110).aspx)