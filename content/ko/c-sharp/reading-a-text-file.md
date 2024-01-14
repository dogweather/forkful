---
title:                "C#: 텍스트 파일 읽기"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

"왜 텍스트 파일을 읽어야 할까요?"

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 요소입니다. 이를 통해 우리는 데이터를 읽고 분석하고 다양한 작업을 수행할 수 있습니다. 또한 많은 애플리케이션에서는 사용자의 입력이나 설정 정보를 저장하기 위해 텍스트 파일을 사용합니다. 따라서 텍스트 파일을 읽는 방법을 익히는 것은 매우 중요합니다.

"어떻게 텍스트 파일을 읽을 수 있을까요?"

텍스트 파일을 읽는 것은 C#에서 매우 간단합니다. 먼저 `StreamReader` 클래스를 사용하여 파일을 열고, `ReadLine()` 메서드를 사용하여 한 줄씩 파일을 읽어올 수 있습니다. 아래는 간단한 예시 코드입니다.

```C#
// 파일 경로를 지정하여 StreamReader 객체 생성
StreamReader file = new StreamReader("test.txt");
string line;

// 한 줄씩 파일을 읽어오기 위해 반복문 사용
while ((line = file.ReadLine()) != null)
{
    // 읽어온 줄을 출력
    Console.WriteLine(line);
}

// 작업이 끝나면 파일을 닫는 것을 잊지 마세요
file.Close();
```

위 예시 코드를 실행하면 `test.txt` 파일의 내용이 한 줄씩 출력될 것입니다. 이처럼 `StreamReader` 클래스를 사용하면 텍스트 파일을 쉽게 읽을 수 있습니다.

"깊게 들어가보기"

텍스트 파일을 읽는 과정에서 더 깊이 들어가보면 `StreamReader` 클래스의 `Peek()` 메서드를 사용하여 현재 위치에서 다음 문자를 읽어올 수 있습니다. 또한 `Read()` 메서드를 사용하여 다음 문자를 읽고 파일의 위치를 이동할 수도 있습니다. 이 외에도 `StreamReader` 클래스에는 다양한 유용한 메서드들이 있으니 참고하면 좋습니다.

"See Also"

- [StreamReader 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader?view=net-5.0)
- [C# 파일 읽기와 쓰기](https://www.csharpstudy.com/Development/fd-open.aspx)
- [C# 파일 처리 방법](https://www.c-sharpcorner.com/uploadfile/puranindia/file-handling-in-C-Sharp/)