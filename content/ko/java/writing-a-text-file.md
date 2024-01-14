---
title:    "Java: 텍스트 파일 작성하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

누군가 텍스트 파일을 쓰는 것에 참여하는 이유는 무엇인가요? 텍스트 파일을 사용하는 것은 프로그램에서 데이터를 저장하고 불러오는 가장 일반적이고 쉬운 방법입니다. 따라서 텍스트 파일 쓰기는 프로그래밍에서 필수적인 기술입니다.

## 하는 방법

자바에서 텍스트 파일을 쓰는 방법은 매우 간단합니다. 먼저 `BufferedWriter`와 `FileWriter` 클래스를 이용해 쓰기 스트림을 생성합니다. 그리고 `write()` 메소드를 통해 문자열을 쓸 수 있습니다. 예제 코드를 살펴보겠습니다.

```java
BufferedWriter writer = new BufferedWriter(new FileWriter("example.txt"));
String text = "안녕하세요, 여러분!";
writer.write(text);
```

위의 예제 코드에서 `BufferedWriter` 클래스는 버퍼링을 통해 효율적으로 파일을 쓰기 위해 사용되고, `FileWriter` 클래스는 실제로 파일에 쓰는 역할을 합니다. `write()` 메소드는 지정한 문자열을 파일에 쓰는 역할을 하며, 파일에 쓰기가 끝나면 `close()` 메소드를 호출해야 합니다.

코드 실행 후 `example.txt` 파일을 열어보면 해당 내용이 적혀있는 것을 확인할 수 있습니다. 만약 파일이 존재하지 않는 경우 자동으로 생성합니다.

## 깊게 파고들기

텍스트 파일을 쓰는 데 더 많은 기능을 추가할 수 있습니다. 예를 들어, `append()` 메소드를 사용하면 파일에 내용을 추가할 수 있습니다. 또한 `newLine()` 메소드를 이용해 줄바꿈을 할 수 있습니다.

또 다른 중요한 기능은 예외처리입니다. 프로그래밍에서는 항상 예외가 발생할 수 있다는 것을 염두해 두어야 합니다. 따라서 `try-catch`문을 이용해 예외를 처리할 수 있도록 코드를 작성하는 것이 좋습니다.

더 자세한 내용은 자바 공식 문서를 참고해주시기 바랍니다.

## 같이 보기

- [자바 공식 문서 - BufferedWriter](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
- [자바 공식 문서 - FileWriter](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [자바 공식 문서 - 예외처리](https://docs.oracle.com/javase/tutorial/essential/exceptions/)