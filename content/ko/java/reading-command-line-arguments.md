---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

커맨드 라인 인수 읽기는 사용자가 프로그램을 실행할 때 전달한 인수를 프로그램에서 사용할 수 있도록하는 프로세스입니다. 프로그래머들이 이를 통해 동적으로 프로그램을 제어하고 사용자 지정을 하기 위해 이것을 사용합니다.

## 사용 방법:

```Java
public class Main {
    public static void main(String[] args) {
        for(String arg : args){
            System.out.println("인수: " + arg);
        }
    }
}
```
예시실행 결과:
```bash
> java Main 안녕하세요 세상
인수: 안녕하세요
인수: 세상
```

## 심화학습

커맨드 라인 인수는 가장 오래된 프로그래밍 인테페이스 중 하나이며, 사용자가 프로그램 실행 시 행동을 제어할 수 있게 합니다. 이것이 가능한 대안으로 사용자 입력, 구성 파일 또는 환경 변수가 있습니다. Java에서는 `main` 메소드의 인수(즉, 문자열 배열인 `args`)로 전달되어 구현됩니다.

## 관련 참고자료

- [Java 공식 도큐멘트](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html): `args` 배열에 대한 자세한 정보와 예제를 제공합니다.
- [Stack Overflow](https://stackoverflow.com/questions/367706/how-to-get-arguments-into-a-java-application-from-outside-it): 외부에서 Java 애플리케이션으로 인수를 가져오는 방법에 대한 토론.