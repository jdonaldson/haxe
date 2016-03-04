local function _hx_clamp(x)
  return (x & 2147483647) - (x & 2147483648)
end
