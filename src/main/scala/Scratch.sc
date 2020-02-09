import fp.{OptionExcercises, OptionUtil}

OptionExcercises.variance(Seq(1, 2, 3))


OptionUtil.map2(fp.Some(2), fp.Some(2))((a, b) => a + b)
OptionUtil.map2(fp.None, fp.Some(2))((a: Int, b) => a + b)
OptionUtil.map2(fp.Some(2), fp.None)((a, b: Int) => a + b)
OptionUtil.map2(fp.None, fp.None)((a: Int, b: Int) => a + b)

