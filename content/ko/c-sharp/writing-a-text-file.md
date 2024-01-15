---
title:                "텍스트 파일 작성하기"
html_title:           "C#: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 작성하는 이유는 여러 가지가 있지만, 가장 일반적인 이유는 데이터나 정보를 저장하기 위해서입니다. 프로그램에서는 종종 텍스트 파일을 사용하여 사용자의 입력이나 출력, 설정 등을 저장하고 불러올 수 있습니다.

## 만들기

텍스트 파일을 만드는 방법은 간단합니다. 먼저 `StreamWriter` 클래스를 사용하여 새 파일을 생성하고, `WriteLine` 메서드를 사용하여 원하는 내용을 파일에 추가합니다. 마지막으로 `Close` 메서드를 사용하여 파일을 닫아주면 됩니다.

```C#
using System.IO; // IO 네임스페이스를 추가해줘야 합니다.

// 파일을 생성합니다.
StreamWriter file = new StreamWriter("example.txt");

// 내용을 파일에 추가합니다.
file.WriteLine("여러분, 안녕하세요!");
file.WriteLine("저는 텍스트 파일 쓰기 예제입니다.");

// 파일을 닫습니다.
file.Close();
```

위의 예제를 실행하고 `example.txt` 파일을 열어보면, 아래와 같이 내용이 저장된 것을 확인할 수 있습니다.

```
여러분, 안녕하세요!
저는 텍스트 파일 쓰기 예제입니다.
```

## 깊게 들어가기

사실, 텍스트 파일을 생성하는 과정은 매우 간단합니다. 하지만 여러분이 사용하는 파일의 형식이나 인코딩에 따라 다를 수 있습니다. 예를 들어, 유니코드를 지원하는 UTF-8 형식으로 파일을 생성하고 싶다면 `StreamWriter` 클래스 생성자에서 두 번째 매개변수로 `Encoding.UTF8`을 전달해주면 됩니다.

또한, 파일을 읽어올 때도 사용하는 `StreamReader` 클래스가 있습니다. 이 클래스를 사용하면 파일에서 한 줄씩 읽어오거나 전체 내용을 문자열로 가져올 수 있습니다. 더 자세한 내용은 관련 문서를 참고하시기 바랍니다.

# 참고

- StreamWriter 클래스 문서: https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamwriter
- StreamReader 클래스 문서: https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader