# extraquantstrat
a R package provide extra functions of quantstrat

未考虑到交易信号是否被执行

version 0.1.0 
修改止损单进入方式，从交易规则部分独立出来，使止损方式更易理解，并便于调整
在applySignal之后，根据交易信号列计算止损指标信号列。

增加函数：

add.stoplosssig

add.stoplossrule

stopLimit

stopPercent

stopTargetPeriod

applyStoplossSig

修改：

strategy 改为 strategy2，结构中增加stoploss

applyStrategy 改为 apply_Strategy，在函数中调用applyRules加入applyStoplossSig

