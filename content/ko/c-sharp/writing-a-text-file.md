---
title:    "C#: 텍스트 파일 작성하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 작성하는 이유는 프로그래밍에서 중요한 부분입니다. 텍스트 파일은 데이터를 저장하고 유지하는 데 사용되는 일반적인 파일 형식입니다.

## 사용 방법
텍스트 파일을 작성하는 가장 간단한 방법은 C#에서 `StreamWriter` 클래스를 사용하는 것입니다. 새 파일을 만들고 데이터를 입력하려면 다음과 같이 작성할 수 있습니다.

```C#
// 파일 생성
StreamWriter writer = new StreamWriter("sample.txt");

// 데이터 입력
writer.WriteLine("안녕하세요!");
writer.WriteLine("저는 프로그래머입니다.");

// 파일 닫기
writer.Close();
```

위 예제 코드를 실행하면 "sample.txt" 라는 파일이 만들어지고 텍스트가 입력될 것입니다. 파일을 만든 후에는 `StreamWriter` 객체를 닫아야 합니다.

## 깊게 파고들기
`StreamWriter` 클래스는 파일을 쓰는 데 사용됩니다. 일반적으로 데이터를 저장하기 위해 사용하는 `StreamWriter` 클래스의 메서드는 다음과 같습니다.

- `Write()`: 문자열을 기록합니다.
- `WriteLine()`: 문자열 뒤에 줄 바꿈 문자를 추가하여 기록합니다.
- `Flush()`: 버퍼의 모든 내용을 파일로 작성합니다.

또한 `File` 클래스의 `WriteAllText()` 메서드를 사용하여 텍스트 파일을 쉽게 작성할 수도 있습니다. 이 방법은 다음과 같이 사용할 수 있습니다.

```C#
// 파일 생성
File.WriteAllText("sample.txt", "안녕하세요! 저는 프로그래머입니다.");
```

`StreamWriter` 클래스나 `File` 클래스의 메서드들은 파일 내용을 덮어쓰게 됩니다. 이들의 오버로드된 메서드를 사용하여 파일의 내용을 추가할 수도 있습니다.

## 참고 자료
- [StreamWriter 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamwriter)
- [File 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.file)
- [Markdown 문법 가이드](https://www.markdownguide.org/)