local function _hx_clamp(x)
  return _G.bit.band(x,2147483647) - _G.bit.band(x,2147483648)
end
