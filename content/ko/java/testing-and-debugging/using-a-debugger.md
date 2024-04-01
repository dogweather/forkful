---
date: 2024-01-26 03:50:13.544497-07:00
description: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uCF54\uB4DC\uC758 \uBC84\uADF8\uB97C \uD14C\uC2A4\uD2B8\uD558\uACE0 \uC218\uC815\
  \uD558\uAE30 \uC704\uD574 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\
  \uB97C \uD1B5\uD574 \uC790\uC2E0\uB4E4\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\
  \ \uD750\uB984\uC744 \uC774\uD574\uD558\uACE0, \uC624\uB958\uC758 \uC6D0\uC778\uC744\
  \ \uC815\uD655\uD788 \uD30C\uC545\uD558\uBA70, \uC2E4\uD589 \uC911\uC778 \uB85C\uC9C1\
  \uC744 \uAC80\uC99D\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.055362-06:00'
model: gpt-4-0125-preview
summary: "\uB514\uBC84\uAC70\uB97C \uC0AC\uC6A9\uD55C\uB2E4\uB294 \uAC83\uC740 \uCF54\
  \uB4DC\uC758 \uBC84\uADF8\uB97C \uD14C\uC2A4\uD2B8\uD558\uACE0 \uC218\uC815\uD558\
  \uAE30 \uC704\uD574 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C\
  \ \uD1B5\uD574 \uC790\uC2E0\uB4E4\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uD750\
  \uB984\uC744 \uC774\uD574\uD558\uACE0, \uC624\uB958\uC758 \uC6D0\uC778\uC744 \uC815\
  \uD655\uD788 \uD30C\uC545\uD558\uBA70, \uC2E4\uD589 \uC911\uC778 \uB85C\uC9C1\uC744\
  \ \uAC80\uC99D\uD569\uB2C8\uB2E4."
title: "\uB514\uBC84\uAC70 \uC0AC\uC6A9\uD558\uAE30"
---

## 방법:
간단한 자바 프로그램이 문제를 일으키고 있고 그 원인을 파악할 수 없다고 가정해 보겠습니다. 자바 개발을 위한 인기 있는 IDE 중 하나인 Eclipse를 사용하여 디버거를 시작하는 방법은 다음과 같습니다:

먼저, 중단점을 설정했는지 확인합니다. 그런 다음 파일을 마우스 오른쪽 버튼으로 클릭하고 'Debug As'를 선택한 다음 'Java Application'을 클릭합니다.

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // 여기에 중단점을 설정하세요
        int result = divide(a, b);
        System.out.println("결과는: " + result);
    }

    private static int divide(int 분자, int 분모) {
        // 또 다른 좋은 중단점 위치
        return 분자 / 분모;
    }
}
```

이렇게 하면 프로그램이 중단점에서 일시 중지되며, 변수를 조사하고, 코드를 한 줄씩 진행하며, 프로그램이 어떻게 동작하는지 관찰할 수 있습니다.

디버거 콘솔에서의 샘플 출력:
```
중단점 도달, 줄: int result = divide(a, b);
```

## 깊이 들어가기
디버깅 개념은 프로그래밍 초기 단계에서부터 존재해 왔습니다. 전설에 따르면 "버그"라는 용어는 실제로 필드의 선구자인 그레이스 호퍼가 컴퓨터 내부에서 발견한 진짜 나방에서 유래했다고 합니다. 오늘날로 빨리 가면 IntelliJ IDEA, Eclipse, NetBeans와 같이 강력한 디버거를 갖춘 정교한 IDE가 있습니다.

IDE 디버거에 대한 대안은 로깅, 출력문(가난한 사람의 디버거), 단언문, 그리고 자바 개발 키트(JDK)의 일부인 jdb(Java Debugger)와 같은 독립 실행형 디버깅 도구를 포함합니다.

디버거는 프로그래머가 실행을 일시 중지(중단점), 코드를 단계별로 진행, 변수 값 검사, 그 값들을 실시간으로 수정하고, 심지어 코드 블록을 블록 단위로 실행할 수 있게 함으로써 작동합니다. 복잡한 애플리케이션을 개발할 때 문제를 일으키는 정확한 코드 줄을 찾는 것이 건초더미에서 바늘을 찾는 것과 같을 수 있어 디버거 사용은 매우 가치 있는 기술로 여겨집니다.

## 참고
- 디버깅에 관한 공식 오라클 문서: [Oracle Java SE Debugging](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- 이클립스의 디버깅 가이드: [Eclipse Debugging Tips](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- 여러 명령줄 JDK 도구를 통합하고 가벼운 프로파일링 기능을 제공하는 시각적 도구인 VisualVM: [VisualVM](https://visualvm.github.io/)
