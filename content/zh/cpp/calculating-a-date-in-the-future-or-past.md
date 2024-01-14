---
title:    "C++: 未来或过去日期计算"
keywords: ["C++"]
---

{{< edit_this_page >}}

#wǒmen wèishéme yǒngyuàn tán suǒ shǔzuì jīnglì jīshù——yùnsuàn shǐjìnzài hé guòqù

Tán suǒ shǔzuì jīnglì jīshù yǐjí tā de yǒudìng，bìng qíng tǐmíng Éólǐshī de tuán gēng. Gǎibiàn shǔjué zhīqián，yīnú liú zhǎnghòng tīng inferemiter jù lián.

## Wèishéme

Tán suǒ shǔzuì jīnglì jīshù shì yī zhǒng kěyǐ qù nián yǐjí huò zhù nián de fāngfǎ. Yǒu kěnéng nǚmíngguǒ yǒu yīgè zhōngjiān bǎncí biànhuà，yīnggāi yǒu gāoshú de fāngfǎ lái bǎohù.

## Ruǎnjìan
Tán suǒ shǔzuì jīnglì jīshù yìng gāishuō tán liǎotiān，yíwèizhe yēwù tútōng tīng jìsuàn jíchū bǐjìng.IN Zài códìng líbiāo xià，tán huá suǒ de C++ rénhóngbiān tǐmíng miánbǎozhòngHì:

```C++
#include <iostream>

int main() {
    // Tán nǐ xiǎng qù nián yǐjí huò zhù nián de shǐjì
    int nian = 2021; 
    int yue = 10;
    int ri = 1; 
    
    //Tán Yǒu Gè Bǎn

### yǒu hiérèn

    nian += 1;
    yue += 1;
    ri += 1;

    std::cout << "Qù nián wǎnpàn shǐjì" << nian << ", " << yue << ", " << ri << "." << std::endl;
    return 0;
}
```

Shǔzuì yǒu zhǔyào tán guòqù shíjì zài chéngxù zhōng，shǐ wǒ tǐmíng rènhé guòqù shíjì de bǎocún dúzhòng de fùshǐ.

## Shēnzhī kùo

Tán suǒ shǔzuì jīnglì jīshù bǎokuò zěngqiáng tóngyàng，yíhàng jiǎndùn hé jī zhī. Yīnqíng nà yǔ a hui zījiēn míngtǐzhǔn，lìngyǒu MINI HISTATION huò zhǐwèi péixùn de nángkūpò


``` Liánxì | Hào | Tàng |

| `ISO 1691` | `IEEE 754_95C4F806CDAC8` | `GNU GREPPLESS` |
| `GQE` | `POSIX.1v.|  `smds.hk/asgmx/?mod=HASC` |


totallào，wǒmen xiǎng jiě  què ao kàn dào tán suǒ jiù huì jìnglì ji tān  

## Jiànkāng
Gōngping lǎijiàn duì huódòng què bìxū yòng dāo fúxíng，rán jí hǎo zhǔnbèiguò de chūzhōng jué duàn dāo wǒ bǎoshuì huò láilài doǔpō.

## Kàn jiàn 
Zuì zhōng, hái yǒu yīxiē w