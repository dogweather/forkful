---
date: 2024-01-20 17:56:33.062473-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBA85\uB839\uC904\
  \ \uC778\uC790\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uC624\uB798\uB41C \uAD00\
  \uD589\uC785\uB2C8\uB2E4. Unix \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uC2DC\uC791\uD574\
  , \uC0AC\uC6A9\uC790\uAC00 \uC178\uC5D0\uC11C \uD504\uB85C\uADF8\uB7A8\uC744 \uC2E4\
  \uD589\uD560 \uB54C \uC635\uC158\uC744 \uC804\uB2EC\uD558\uAC8C \uD5C8\uC6A9\uD588\
  \uC2B5\uB2C8\uB2E4. Java\uC5D0\uC11C `main` \uBA54\uC18C\uB4DC\uC758 `String[] args`\
  \ \uD30C\uB77C\uBBF8\uD130\uB97C \uD1B5\uD574 \uC778\uC790\uB4E4\uC774\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.446752-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uBA85\uB839\uC904 \uC778\uC790\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 \uC624\uB798\uB41C \uAD00\uD589\uC785\
  \uB2C8\uB2E4."
title: "\uBA85\uB839\uC904 \uC778\uC218 \uC77D\uAE30"
weight: 23
---

## How to: (어떻게 하나요?)
```java
public class CommandLineReader {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("인자를 받았습니다:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("인자가 없습니다.");
        }
    }
}
```
터미널에서 다음 명령으로 실행:
```
java CommandLineReader 이것은 테스트입니다
```
출력:
```
인자를 받았습니다:
이것은
테스트입니다
```

## Deep Dive (심층 분석)
명령줄 인자를 사용하는 것은 오래된 관행입니다. Unix 시스템에서 시작해, 사용자가 셸에서 프로그램을 실행할 때 옵션을 전달하게 허용했습니다. Java에서 `main` 메소드의 `String[] args` 파라미터를 통해 인자들이 전달됩니다. 이 배열에는 각각의 인자가 공백으로 구분되어 저장됩니다. Java 5부터 `Varargs` 사용이 가능해졌으며, 이를 통해 개발자들은 가변 길이 인자 목록을 메소드에 전달할 수 있게 되었습니다. 대안으로, Apache Commons CLI와 같은 라이브러리를 사용하여 파싱을 더 편리하게 할 수도 있습니다. 가장 중요한 구현 세부 사항은 안전하게 빈 인자 배열을 처리하고, 각 인자를 적절히 파싱하는 것입니다.

## See Also (추가 정보)
- [Oracle Java Documentation](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI Library](https://commons.apache.org/proper/commons-cli/)
- [Java Varargs Guide](https://docs.oracle.com/javase/8/docs/technotes/guides/language/varargs.html)
