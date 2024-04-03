---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:07.312355-07:00
description: "\uBC29\uBC95: \uC790\uBC14\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\uD558\uB294 \uBC29\uBC95\uC740 \uC8FC\
  \uB85C `java.nio.file.Files`\uC640 `java.io.File` \uD074\uB798\uC2A4\uB97C \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\uB2E4. **`java.nio.file.Files` \uC0AC\uC6A9\
  \uD558\uAE30**: \uC774 \uBC29\uBC95\uC740 \uCD5C\uADFC \uC790\uBC14 \uBC84\uC804\
  \uC5D0\uC11C \uAD8C\uC7A5\uB429\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.070322-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC5D0\uC11C \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\
  \uB294\uC9C0 \uD655\uC778\uD558\uB294 \uBC29\uBC95\uC740 \uC8FC\uB85C `java.nio.file.Files`\uC640\
  \ `java.io.File` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC785\uB2C8\
  \uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 방법:
자바에서 디렉토리가 존재하는지 확인하는 방법은 주로 `java.nio.file.Files`와 `java.io.File` 클래스를 사용하는 것입니다.

**`java.nio.file.Files` 사용하기**:

이 방법은 최근 자바 버전에서 권장됩니다.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // 여기에 디렉토리 경로를 지정하세요
        String directoryPath = "path/to/directory";

        // 디렉토리가 존재하는지 확인
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("디렉토리가 존재합니다.");
        } else {
            System.out.println("디렉토리가 존재하지 않습니다.");
        }
    }
}
```
**샘플 출력**:
```
디렉토리가 존재합니다.
```
또는 
```
디렉토리가 존재하지 않습니다.
```

**`java.io.File` 사용하기**:

`java.nio.file.Files`가 권장되는 것과는 달리, 오래된 `java.io.File` 클래스를 사용할 수도 있습니다.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // 여기에 디렉토리 경로를 지정하세요
        String directoryPath = "path/to/directory";

        // File 객체 생성
        File directory = new File(directoryPath);

        // 디렉토리가 존재하는지 확인
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("디렉토리가 존재합니다.");
        } else {
            System.out.println("디렉토리가 존재하지 않습니다.");
        }
    }
}
```
**샘플 출력**:
```
디렉토리가 존재합니다.
```
또는
```
디렉토리가 존재하지 않습니다.
```

**제3자 라이브러리 사용하기**:

표준 자바 라이브러리만으로도 이 작업에 주로 충분하지만, Apache Commons IO와 같은 제3자 라이브러리는 더 복잡한 응용 프로그램에서 유용할 수 있는 추가적인 파일 처리 유틸리티를 제공합니다.

**Apache Commons IO**:

먼저, 프로젝트에 Apache Commons IO 의존성을 추가하세요. 그 후로, 그것의 특징을 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다.

```java
// Apache Commons IO가 프로젝트에 추가된 것으로 가정

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // 여기에 디렉토리 경로를 지정하세요
        String directoryPath = "path/to/directory";

        // FileUtils를 사용하여 확인
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("디렉토리가 존재합니다.");
        } else {
            System.out.println("디렉토리가 존재하지 않습니다.");
        }
    }
}
```

**주의**: `FileUtils.directoryContains`는 디렉토리가 특정 파일을 포함하는지 확인합니다만, 두 번째 인수로 `null`을 전달함으로써 디렉토리의 존재 여부를 확인하는 데 사용할 수 있습니다. 이 방법이 항상 직관적이거나 의도된 사용법은 아닐 수 있으니 주의하세요.

**샘플 출력**:
```
디렉토리가 존재합니다.
```
또는
```
디렉토리가 존재하지 않습니다.
```
