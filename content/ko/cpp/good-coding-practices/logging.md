---
date: 2024-01-26 01:00:19.908058-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C\uC758 \uB85C\uAE45\uC740\
  \ \uC774\uBCA4\uD2B8, \uC0C1\uD0DC \uBC0F \uC815\uBCF4\uB97C \uD30C\uC77C\uC774\uB098\
  \ \uAE30\uD0C0 \uCD9C\uB825 \uB9E4\uCCB4\uC5D0 \uAE30\uB85D\uD558\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uC2E0\uC758\
  \ \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uBB34\uC2A8 \uC77C\uC774 \uC77C\
  \uC5B4\uB098\uACE0 \uC788\uB294\uC9C0 \uCD94\uC801\uD558\uACE0, \uBB38\uC81C\uB97C\
  \ \uB514\uBC84\uADF8\uD558\uACE0, \uD5A5\uD6C4 \uBD84\uC11D \uBC0F \uCD5C\uC801\uD654\
  \uB97C \uC704\uD574 \uC131\uB2A5\uC744 \uBAA8\uB2C8\uD130\uB9C1\uD558\uAE30 \uC704\
  \uD574 \uB85C\uAE45\uC744 \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.678486-06:00'
model: gpt-4-1106-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C\uC758 \uB85C\uAE45\uC740 \uC774\
  \uBCA4\uD2B8, \uC0C1\uD0DC \uBC0F \uC815\uBCF4\uB97C \uD30C\uC77C\uC774\uB098 \uAE30\
  \uD0C0 \uCD9C\uB825 \uB9E4\uCCB4\uC5D0 \uAE30\uB85D\uD558\uB294 \uACFC\uC815\uC785\
  \uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 무엇 & 왜?
프로그래밍에서의 로깅은 이벤트, 상태 및 정보를 파일이나 기타 출력 매체에 기록하는 과정입니다. 프로그래머들은 자신의 애플리케이션에서 무슨 일이 일어나고 있는지 추적하고, 문제를 디버그하고, 향후 분석 및 최적화를 위해 성능을 모니터링하기 위해 로깅을 합니다.

## 방법:
리눅스 박스에서 작업 중이고 좋은 옛날 C++로 로그를 파일에 넣고 싶다고 가정해 보겠습니다. 파일 작업을 하기 위해 `<iostream>`과 `<fstream>` 라이브러리를 포함해야 합니다. 여기 간단한 예제가 있습니다:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // 추가 모드로 열기

    if (!logFile.is_open()) {
        std::cerr << "로그 파일을 여는 데 문제가 있습니다!" << std::endl;
        return 1;
    }

    logFile << "애플리케이션 시작" << std::endl;
  
    // ... 앱 로직 중 어딘가에서
    logFile << "중요한 이벤트가 발생했습니다" << std::endl;

    // 파일 스트림을 닫는 것을 잊지 마세요
    logFile.close();

    return 0;
}
```

`tail -f appLog.txt`로 로그 파일을 테일링하면 다음과 같이 표시됩니다:

```
애플리케이션 시작
중요한 이벤트가 발생했습니다
```

정리되었습니다, 이벤트의 타임스탬프 기록을 가지게 되었군요!

## 심층 분석
로그는 컴퓨팅 자체만큼 오래되었고, 고대 컴퓨터가 무엇을 하고 있는지 추적하기 위한 종이 위의 실제 표시에서 그 뿌리를 찾을 수 있습니다. 현대에는 모두 정교한 소프트웨어 솔루션에 관한 것입니다. 위의 간단하고 더러운 예제처럼 바로 파일로 로깅할 수도 있고, C++ 영역에서 Log4cpp나 Boost.Log 같은 멋지고 정교한 로깅 프레임워크를 사용할 수도 있습니다; 이런 벤처들은 로깅 레벨, 포맷 제어 등을 제공합니다.

레벨에 대해 말하자면, 로깅 모범 사례는 서로 다른 심각도 수준의 로깅을 사용하는 것을 포함합니다—정보, 디버그, 경고, 오류, 치명적—버그를 제거하거나 앱이 왜 심술궂은 청소년처럼 행동하는지 파악할 때 소음을 필터링할 수 있습니다.

성능 측면에서, 로그에 대해 게을러지지 마세요. 과도한 로깅은 번개처럼 빠른 앱을 느릿느릿한 달팽이 경주로 바꿀 수 있고, 파일 시스템을 느려지게 하거나, 클라우드 기반이라면 저장 공간 요금으로 비용을 들게 할 수도 있습니다. 적절한 균형을 찾는 것이 중요합니다: 필요한 것만 기록하고, 그 이상은 하지 마세요.

## 또한 보기
로깅 습관을 더욱 발전시키고 싶은 분들을 위한 몇가지 자료입니다:

- [Boost.Log 라이브러리](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html)는 고급 로깅 기능을 위한 것입니다.
- 구글의 [glog 라이브러리](https://github.com/google/glog)는 기술 거인이 로깅하는 데 사용하는 것입니다.
- [Log4cpp 라이브러리](http://log4cpp.sourceforge.net/)는 설정 가능한 로깅 메커니즘을 제공합니다.

그리고 로깅의 배경과 방법에 대해 좀 더 이해하고 싶다면, 다음을 읽어보세요:

- 스택 오버플로우 스레드에서 [로깅 모범 사례](https://stackoverflow.com/questions/783956/logging-best-practices)는 이 주제에 대한 동료 심사를 통한 심층 분석을 제공합니다.
